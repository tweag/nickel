{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
  inputs.pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  inputs.pre-commit-hooks.inputs.flake-utils.follows = "flake-utils";
  inputs.import-cargo.url = "github:edolstra/import-cargo";
  # Use this fork rather than the official because of a not-yet-upstreamed bug fix for WASM
  # See https://www.tweag.io/blog/2022-09-22-rust-nix/
  inputs.cargo2nix.url = "github:cargo2nix/cargo2nix";
  inputs.cargo2nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.cargo2nix.inputs.rust-overlay.follows = "rust-overlay";
  inputs.cargo2nix.inputs.flake-utils.follows = "flake-utils";
  # We have to add `rust-overlay` to override the input `nixpkgs` rather than
  # inputs.cargo2nix.inputs.rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
  # because of https://github.com/NixOS/nix/issues/5790
  inputs.rust-overlay = {
    url = "github:oxalica/rust-overlay";
    inputs.nixpkgs.follows = "nixpkgs";
    inputs.flake-utils.follows = "flake-utils";
  };

  nixConfig = {
    extra-substituters = [ "https://tweag-nickel.cachix.org" ];
    extra-trusted-public-keys = [ "tweag-nickel.cachix.org-1:GIthuiK4LRgnW64ALYEoioVUQBWs0jexyoYVeLDBwRA=" ];
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , import-cargo
    , cargo2nix
    , rust-overlay
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
          customOverlay
          cargo2nix.overlays.default
        ];
      };

      # We need a different `pkgs` for WASM
      # See https://www.tweag.io/blog/2022-09-22-rust-nix/
      # pkgsWasm = import nixpkgs {
      #   inherit system;
      #   crossSystem = {
      #     system = "wasm32-wasi";
      #     useLLVM = true;
      #   };
      #   overlays = [
      #     customOverlay
      #     cargo2nix.overlays.default
      #   ];
      # };

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
        , extraRustComponents ? [ "rust-src" "rust-analysis" "rustfmt" "clippy" ]
        , rustChannel ? "stable"
        , rustVersion ? "latest"
        , target ? pkgs.rust.toRustTarget pkgs.stdenv.hostPlatform
        }:
        pkgs.rustBuilder.makePackageSet {
          inherit rustProfile extraRustComponents rustChannel rustVersion target;
          # TODO Generate and compare Cargo.nix in CI. Maybe pre-commit hook?
          packageFun = import ./Cargo.nix;
        };

      # In the future we may want to also build WASM target with `cargo2nix` but for now it fails while building dependencies, e.g.
      # crate-fd-lock> error[E0255]: the name `unsupported` is defined multiple times
      # crate-fd-lock>   --> src/sys/mod.rs:15:17
      # crate-fd-lock>    |
      # crate-fd-lock> 14 |         mod unsupported;
      # crate-fd-lock>    |         ---------------- previous definition of the module `unsupported` here
      # crate-fd-lock> 15 |         pub use unsupported;
      # crate-fd-lock>    |                 ^^^^^^^^^^^ `unsupported` reimported here
      # crate-fd-lock>    |
      # crate-fd-lock>    = note: `unsupported` must be defined only once in the type namespace of this module
      # crate-fd-lock> help: you can use `as` to change the binding name of the import
      # crate-fd-lock>    |
      # crate-fd-lock> 15 |         pub use unsupported as other_unsupported;
      # crate-fd-lock>    |                 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # mkRustWasm =
      #   { rustProfile ? "minimal"
      #   , extraRustComponents ? [ "rust-src" "rust-analysis" "rustfmt-preview" "clippy-preview" ]
      #   , rustChannel ? "stable"
      #   , rustVersion ? "latest"
      #   , target ? "wasm32-unknown-unknown"
      #   }:
      #   pkgsWasm.rustBuilder.makePackageSet {
      #     inherit rustProfile extraRustComponents rustChannel rustVersion target;
      #     # TODO Generate and compare Cargo.nix in CI. Maybe pre-commit hook?
      #     # cargo2nix thinks we're building for wasm32-unknown-wasi now.
      #     # We need to guide it to wasm32-unknown-unknown instead.
      #     # See https://www.tweag.io/blog/2022-09-22-rust-nix/
      #     packageFun = attrs: import ./Cargo.nix (attrs // {
      #       hostPlatform = attrs.hostPlatform // {
      #         parsed = attrs.hostPlatform.parsed // {
      #           kernel.name = "unknown";
      #         };
      #       };
      #     });
      #   };
      mkRustWasm =
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
        , nickel ? buildNickel {
            inherit rust;
          }
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
              entry =
                pkgs.lib.mkForce "${pkgs.writeShellScript "rustfmt" ''
                    PATH=$PATH:${rust.rustToolchain}/bin
                    ${rust.rustToolchain}/bin/cargo fmt -- --check --color always
                  ''}";
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
                  echo "echo buildNickel inputDerivation"
                  echo ${(buildNickel {inherit rust;}).bin.inputDerivation}
                  source ${(buildNickel {inherit rust;}).bin.inputDerivation}
                  NIX_BUILD_TOP=$nix_build_top
                  TEMP=$nix_build_top
                  TEMPDIR=$nix_build_top
                  TMP=$nix_build_top
                  TMPDIR=$nix_build_top
                  HOME=$nix_build_top
                  source $stdenv/setup
                '';
              in
              {
                enable = true;
                files = "\\.rs$";
                pass_filenames = false;
                # entry = "${pkgs.nix}/bin/nix --extra-experimental-features nix-command shell --derivation \"${rust.workspaceShell {}}\"";
                entry =
                  "${pkgs.writeShellScript "clippy-hook" ''
                        ${nickelBuildInputsSetup}
                         cargo clippy --workspace ${offlineOpts} -- --no-deps --deny warnings ${allowOpts}
                 ''}";
                # entry =
                #   let
                #     name = "
                # custom-clippy ";
                #         custom-clippy = pkgs.writeShellApplication {
                #           inherit name;
                #           runtimeInputs = [ (rust.workspaceShell { }) ];
                #           text = ''
                #             cargo clippy --workspace ${offlineOpts} -- --no-deps --deny warnings ${allowOpts}
                #           '';
                #         };
                #       in
                #       "${custom-clippy}/bin/${name}";
                # entry = pkgs.runCommand "custom-clippy" { inputsFrom = [ (rust.workspaceShell { }) ]; } ''
                #   cargo clippy --workspace ${offlineOpts} -- --no-deps --deny warnings ${allowOpts}
                # '';
                # entry =
                #   "${pkgs.writeShellScript "clippy-hook" ''
                #         PATH=$PATH:${rust.rustToolchain}/bin
                #         ${rust.rustToolchain}/bin/cargo clippy --workspace ${offlineOpts} -- --no-deps --deny warnings ${allowOpts}
                #       ''}";
              };
          };
        };

      # TODO this is not isofunctional to `master`: the package only contains `nickel`, noth LSP `nls`.
      # TODO checkPhase: `cargo test`
      buildNickel =
        { rust ? mkRust { } }:
        # By default `cargo2nix` sets `.bin.name` to the *package* name rather than the *bin* name (from `Cargo.toml`).
        # This means that e.g. `nix run . -- repl` will fail because it tries to run `<nix path>/bin/nickel-lang` rather than `<nix path>/bin/nickel`.
        # We fix this by setting `meta.mainProgram`.
        # See `nix run --help` for more information on executable name.
        ((rust.workspace.nickel-lang { }).overrideAttrs (_old: { meta.mainProgram = "nickel"; }));

      # See comment on `mkRustWasm`
      # buildNickelWasm =
      #   { rust ? mkRustWasm { }
      #   , optimize ? true
      #   }:
      #   (rust.workspace.nickel-repl { }).bin;
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

      makeDevShell = { rust }: (rust.workspaceShell { }).overrideAttrs (old: {
        buildInputs = old.buildInputs ++ [
          pkgs.rust-analyzer
          pkgs.nodejs
          pkgs.node2nix
          pkgs.nodePackages.markdownlint-cli
        ];

        shellHook = (pre-commit-builder { inherit rust; isHermetic = false; }).shellHook + ''
          echo "== = Nickel development shell == ="
          echo " Info: Git hooks can be installed using \`pre-commit install\`"
        '';
      });
      # makeDevShell = { rust }: pkgs.mkShell {
      # inputsFrom = [ (buildNickel { inherit rust; }).workspaceShell ];
      # buildInputs = [
      #   pkgs.rust-analyzer
      #   pkgs.nodejs
      #   pkgs.node2nix
      #   pkgs.nodePackages.markdownlint-cli
      # ];

      #   shellHook = (pre-commit-builder { inherit rust; isHermetic = false; }).shellHook + ''
      #     echo " == = Nickel development shell ==="
      #     echo "Info: Git hooks can be installed using \`pre-commit install\`"
      #   '';
      # };

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

      # devShells = { default = ((mkRust { }).workspaceShell { }); };
      devShells = {
        default = devShells.stable;
      } // (forEachRustChannel
        (channel: {
          name = channel;
          value = makeDevShell { rust = mkRust { rustChannel = channel; rustProfile = "default"; }; };
        }
        ));

      checks = {
        foo =
          let
            rust = mkRust { };
          in
          pkgs.stdenv.mkDerivation
            {
              name = "foo";
              src = self;
              inputsFrom = (pkgs.lib.mapAttrsToList (_: pkg: pkg { }) rust.noBuild.workspace);
              nativeBuildInputs = [ rust.rustToolchain (rust.noBuild.workspace.nickel-lang { }) ] ++ (with pkgs; [ cacert ]);
              RUST_SRC_PATH = "${rust.rustToolchain}/lib/rustlib/src/rust/library";
              buildPhase = ''
                ls -la
                runHook overrideCargoManifest
                runHook setBuildEnv
                runHook runCargo
                ls -la
                ls -la target
                cargo clippy --workspace --frozen --offline -- --no-deps --deny warnings --allow clippy::new-without-default --allow clippy::match_like_matches_macro
                touch $out
              '';
              dontInstall = true;
            };
        # wasm-opt can take long: eschew optimizations in checks
        wasm = buildNickelWasm { optimize = false; };
        inherit vscodeExtension;
        pre-commit = pre-commit-builder { isHermetic = true; };
      } // (forEachRustChannel (channel:
        {
          name = "nickel-against-${channel}-rust-channel";
          value = buildNickel { rust = mkRust { rustChannel = channel; }; };
        }
      ));
    }
    );
}



