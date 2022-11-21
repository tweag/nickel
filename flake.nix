{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  inputs.pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  inputs.rust-overlay.inputs.flake-utils.follows = "flake-utils";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  nixConfig = {
    extra-substituters = [ "https://tweag-nickel.cachix.org" ];
    extra-trusted-public-keys = [ "tweag-nickel.cachix.org-1:GIthuiK4LRgnW64ALYEoioVUQBWs0jexyoYVeLDBwRA=" ];
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , rust-overlay
    , import-cargo
    }:
    let
      SYSTEMS = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      RUST_CHANNELS = [
        "stable"
        "beta"
        "nightly"
      ];

      forEachRustChannel = fn: builtins.listToAttrs (builtins.map fn RUST_CHANNELS);

      cargoTOML = builtins.fromTOML (builtins.readFile ./Cargo.toml);

      version = "${cargoTOML.package.version}_${builtins.substring 0 8 self.lastModifiedDate}_${self.shortRev or "dirty"}";

      customOverlay = final: prev: {
        # The version of `wasm-bindgen` CLI *must* be the same as the `wasm-bindgen` Rust dependency in `Cargo.toml`.
        # The definition of `wasm-bindgen-cli` in Nixpkgs does not allow overriding directly the attrset passed to `buildRustPackage`.
        # We instead override the attrset that `buildRustPackage` generates and passes to `mkDerivation`.
        # See https://discourse.nixos.org/t/is-it-possible-to-override-cargosha256-in-buildrustpackage/4393
        wasm-bindgen-cli = prev.wasm-bindgen-cli.overrideAttrs (oldAttrs:
          let
            wasmBindgenCargoVersion = cargoTOML.dependencies.wasm-bindgen.version;
            # Remove the pinning `=` prefix of the version
            wasmBindgenVersion = builtins.substring 1 (builtins.stringLength wasmBindgenCargoVersion) wasmBindgenCargoVersion;
          in
          rec {
            pname = "wasm-bindgen-cli";
            version = wasmBindgenVersion;

            src = final.fetchCrate {
              inherit pname version;
              sha256 = "sha256-+PWxeRL5MkIfJtfN3/DjaDlqRgBgWZMa6dBt1Q+lpd0=";
            };

            cargoDeps = oldAttrs.cargoDeps.overrideAttrs (final.lib.const {
              # This `inherit src` is important, otherwise, the old `src` would be used here
              inherit src;
              outputHash = "sha256-GwLeA6xLt7I+NzRaqjwVpt1pzRex1/snq30DPv4FR+g=";
            });
          });
      };

    in
    flake-utils.lib.eachSystem SYSTEMS (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import rust-overlay)
          customOverlay
        ];
      };

      cargoHome = (import-cargo.builders.importCargo {
        lockFile = ./Cargo.lock;
        inherit pkgs;
      }).cargoHome;

      # Additional packages required for some systems to build Nickel
      missingSysPkgs =
        if pkgs.stdenv.isDarwin then
          [
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.libiconv
          ]
        else
          [ ];

      mkRust =
        { rustProfile ? "minimal"
        , rustExtensions ? [
            "rust-src"
            "rust-analysis"
            "rustfmt-preview"
            "clippy-preview"
          ]
        , channel ? "stable"
        , target ? pkgs.rust.toRustTarget pkgs.stdenv.hostPlatform
        }:
        let
          _rust =
            if channel == "nightly" then
              pkgs.rust-bin.selectLatestNightlyWith
                (toolchain: toolchain.${rustProfile}.override {
                  extensions = rustExtensions;
                  targets = [ target ];
                })
            else
              pkgs.rust-bin.${channel}.latest.${rustProfile}.override {
                extensions = rustExtensions;
                targets = [ target ];
              };
        in
        pkgs.buildEnv {
          name = _rust.name;
          inherit (_rust) meta;
          buildInputs = [ pkgs.makeWrapper ];
          paths = [ _rust ];
          pathsToLink = [ "/" "/bin" ];
          # XXX: This is needed because cargo and clippy commands need to
          # also be aware of other binaries in order to work properly.
          # https://github.com/cachix/pre-commit-hooks.nix/issues/126
          postBuild = ''
            for i in $out/bin/*; do
              wrapProgram "$i" --prefix PATH : "$out/bin"
            done

          '';
        };

      pre-commit-builder =
        { rust ? mkRust { }
        , nickel ? buildNickel { inherit rust; }
        , isHermetic
        }: pre-commit-hooks.lib.${system}.run {
          src = self;
          hooks = {
            nixpkgs-fmt = {
              enable = true;
              excludes = [
                "lsp/client-extension/default.nix"
                "lsp/client-extension/node-env.nix"
                "lsp/client-extension/node-packages.nix"
              ];
            };

            rustfmt = {
              enable = true;
              entry = pkgs.lib.mkForce "${rust}/bin/cargo-fmt fmt -- --check --color always";
            };

            markdownlint = {
              enable = true;
              excludes = [
                "notes/(.+)\\.md$"
                "^RELEASES\\.md$"
              ];
            };

            # The default `clippy` pre-commit-hook is too standard, hence we create our own.
            # Compared to the default `clippy` hook, we customize the following:
            # - custom Clippy options
            # - in case of hermetic run (mostly used by CI), we reuse the Nixified Rust dependencies from `nickel`
            custom-clippy =
              let
                allow = [
                  "clippy::new-without-default"
                  "clippy::match_like_matches_macro"
                ];

                allowOpts = builtins.concatStringsSep " "
                  (builtins.map (lint: "--allow \"${lint}\"") allow);

                # Clippy requires access to the crate registry because many lints require dependency information.
                # Similarly to `buildNickel`, we support 2 modes:
                # - when run as a shell hook, we want to be fast, so we let Cargo do incremental compilation, and download dependencies as needed.
                #   This is mainly used by developers, who care about speed at the expense of hermeticity.
                # - when run as a flake check, we want to be fully reproducible, so we Nixify Cargo dependency resolution.
                #   This is mainly used by CI, where the extra time is not such a big deal.
                offlineOpts = pkgs.lib.optionalString isHermetic "--frozen --offline";

                # When running in hermetic mode, all Rust dependencies must be already present.
                # Hence we can't just call `cargo` directly, as it would try to download dependencies.
                # Hence we do the following:
                # - `source ${nickel.inputDerivation}` to set the environment variables `buildInputs` and `stdenv` just like in a Nickel build.
                #   The `buildInputs` will contain the path to Nixified Rust dependencies (a.k.a. `cargoHome`), as well as other build inputs, like `Security` for Darwin.
                #   ⚠️ This will also completely unset `PATH`, and *update* various environment variables used in downstream scripts, like `TMPDIR`.
                # - `source $stdenv/setup` does a lot of Nix magic, including loading in the `PATH` everything in the `buildInputs` environment variable.
                #
                # 💡 The other commands are needed for Darwin builds to undo some changes of `source ${nickel.inputDerivation}`.
                #    For Linux, these extra commands are unnecessary (because their value is "updated" from `/build` to `/build`), though harmless.
                nickelBuildInputsSetup = pkgs.lib.optionalString isHermetic ''
                  nix_build_top=$NIX_BUILD_TOP
                  source ${nickel.inputDerivation}
                  NIX_BUILD_TOP=$nix_build_top
                  TEMP=$nix_build_top
                  TEMPDIR=$nix_build_top
                  TMP=$nix_build_top
                  TMPDIR=$nix_build_top
                  source $stdenv/setup
                '';
              in
              {
                enable = true;
                files = "\\.rs$";
                pass_filenames = false;
                entry =
                  "${pkgs.writeShellScript "clippy-hook" ''
                    ${nickelBuildInputsSetup}
                    cargo clippy --workspace ${offlineOpts} -- --no-deps --deny warnings ${allowOpts}
                  ''}";
              };
          };
        };

      buildNickel =
        { rust ? mkRust { }, withCargoHome ? true }:
        pkgs.stdenv.mkDerivation {
          name = "nickel-${version}";

          buildInputs =
            [ rust ]
            # cargoHome is used for a fully, hermetic, nixified build. This is
            # what we want for e.g. nix-build. However, when hacking on Nickel,
            # we rather provide the necessary tooling but let people use the
            # native way (`cargo build`), because:
            #
            # - we don't care as much about hermeticity when iterating quickly
            #   over the codebase
            # - we get incremental build and a build order of magnitudes
            #   faster
            # - etc.
            ++ pkgs.lib.optional withCargoHome cargoHome
            ++ missingSysPkgs;

          src = self;

          buildPhase = ''
            cargo build --workspace --exclude nickel-repl --release --frozen --offline
          '';

          doCheck = true;

          checkPhase = ''
            cargo test --release --frozen --offline
          '';

          installPhase = ''
            mkdir -p $out
            cargo install --frozen --offline --path . --root $out
            cargo install --frozen --offline --path lsp/nls --root $out
            rm $out/.crates.toml
          '';

        };

      makeDevShell = { rust }: pkgs.mkShell {
        inputsFrom = [ (buildNickel { inherit rust; withCargoHome = false; }) ];
        buildInputs = [
          pkgs.rust-analyzer
          pkgs.nodejs
          pkgs.node2nix
          pkgs.nodePackages.markdownlint-cli
        ];

        shellHook = (pre-commit-builder { inherit rust; isHermetic = false; }).shellHook + ''
          echo "=== Nickel development shell ==="
          echo "Info: Git hooks can be installed using \`pre-commit install\`"
        '';

        RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
      };

      buildNickelWasm =
        { rust ? mkRust { target = "wasm32-unknown-unknown"; }
        , optimize ? true
        }:
        pkgs.stdenv.mkDerivation {
          name = "nickel-wasm-${version}";

          src = self;

          buildInputs = [
            rust
            pkgs.wasm-pack
            pkgs.wasm-bindgen-cli
            pkgs.binaryen
            cargoHome
          ] ++ missingSysPkgs;

          buildPhase = ''
            cd nickel-wasm-repl
            wasm-pack build --mode no-install -- --no-default-features --frozen --offline
            # Because of wasm-pack not using existing wasm-opt
            # (https://github.com/rustwasm/wasm-pack/issues/869), we have to
            # run wasm-opt manually
            echo "[Nix build script] Manually running wasm-opt..."
            wasm-opt ${if optimize then "-O4 " else "-O0"} pkg/nickel_repl_bg.wasm -o pkg/nickel_repl.wasm
          '';

          installPhase = ''
            mkdir -p $out
            cp -r pkg $out/nickel-repl
          '';
        };

      buildDocker = nickel: pkgs.dockerTools.buildLayeredImage {
        name = "nickel";
        tag = version;
        contents = [
          nickel
          pkgs.bashInteractive
        ];
        config = {
          Cmd = "bash";
        };
      };

      vscodeExtension =
        let node-package = (pkgs.callPackage ./lsp/client-extension { }).package;
        in
        (node-package.override rec {
          pname = "nls-client";
          outputs = [ "vsix" "out" ];
          nativeBuildInputs = with pkgs; [
            # `vsce` depends on `keytar`, which depends on `pkg-config` and `libsecret`
            pkg-config
            libsecret
          ];
          postInstall = ''
            npm run compile
            mkdir -p $vsix
            echo y | npx vsce package -o $vsix/${pname}.vsix
          '';
        }).vsix;

      userManual = pkgs.stdenv.mkDerivation {
        name = "nickel-user-manual-${version}";
        src = ./doc/manual;
        installPhase = ''
          mkdir -p $out
          cp -r ./ $out
        '';
      };

      stdlibDoc = pkgs.stdenv.mkDerivation {
        name = "nickel-stdlib-doc-${version}";
        src = ./stdlib;
        installPhase = ''
          mkdir -p $out
          for file in *
          do
            module=$(basename $file .ncl)
            ${self.packages."${system}".default}/bin/nickel doc -f "$module.ncl" \
              --output "$out/$module.md"
          done
        '';
      };

    in
    rec {
      packages = {
        default = packages.build;
        build = buildNickel { };
        buildWasm = buildNickelWasm { };
        dockerImage = buildDocker packages.build; # TODO: docker image should be a passthru
        inherit vscodeExtension;
        inherit userManual;
        inherit stdlibDoc;
      };

      devShells = {
        default = devShells.stable;
      } // (forEachRustChannel
        (channel: {
          name = channel;
          value = makeDevShell { rust = mkRust { inherit channel; rustProfile = "default"; }; };
        }
        ));

      checks = {
        # wasm-opt can take long: eschew optimizations in checks
        wasm = buildNickelWasm { optimize = false; };
        inherit vscodeExtension;
        pre-commit = pre-commit-builder { isHermetic = true; };
      } // (forEachRustChannel (channel:
        {
          name = "nickel-against-${channel}-rust-channel";
          value = buildNickel { rust = mkRust { inherit channel; }; };
        }
      ));
    }
    );
}
