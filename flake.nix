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

      buildNickel =
        { channel ? "stable"
        , isDevShell ? false
        , target ? pkgs.rust.toRustTarget pkgs.stdenv.hostPlatform
        ,
        }:
        let
          rustProfile =
            if isDevShell then "default"
            else "minimal";

          rust = mkRust { inherit rustProfile channel target; };

          pre-commit-builder = { isHook }: pre-commit-hooks.lib.${system}.run {
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
                  offlineOpts = if isHook then "" else "--frozen --offline";

                  # Since `pre-commit-hooks` does not execute the entries (commands) as part of a Nix derivation,
                  # we have to do some extra setup work.
                  # - `stdenv` provides standard environment similarly to `mkDerivation`, including a C compiler (`cc`) required by Rust.
                  # - `cargoHome` provides Rust with the right `CARGO_HOME` environment variable to discover built dependencies.
                  cargoHomeHookSetup = if isHook then "" else ''
                    source ${pkgs.stdenv}/setup
                    source ${cargoHome}/nix-support/setup-hook
                  '';
                in
                {
                  enable = true;
                  files = "\\.rs$";
                  pass_filenames = false;
                  entry =
                    "${pkgs.writeShellScript "clippy-hook" ''
                    ${cargoHomeHookSetup}
                    ${rust}/bin/cargo clippy --workspace ${offlineOpts} -- --no-deps --deny warnings ${allowOpts}
                  ''}";
                };
            };
          };

        in
        pkgs.stdenv.mkDerivation {
          name = "nickel-${version}";

          buildInputs =
            [ rust ]
            ++ missingSysPkgs
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
            ++ (if isDevShell then [ pkgs.rust-analyzer pkgs.clippy ] else [ cargoHome ]);

          src = if isDevShell then null else self;

          buildPhase = ''
            cargo build --workspace --exclude nickel-repl --release --frozen --offline
          '';

          doCheck = true;

          checkPhase = ''
            cargo test --release --frozen --offline
          '' + (pkgs.lib.optionalString (channel == "stable") ''
            cargo fmt --all -- --check
          '');

          installPhase = ''
            mkdir -p $out
            cargo install --frozen --offline --path . --root $out
            cargo install --frozen --offline --path lsp/nls --root $out
            rm $out/.crates.toml
          '';

          shellHook = (pre-commit-builder { isHook = true; }).shellHook + ''
            echo "=== Nickel development shell ==="
            echo "Info: Git hooks can be installed using \`pre-commit install\`"
          '';

          # Make the `pre-commit` available in the output derivation as it's used by the `checks` output
          passthru = { pre-commit-inner = pre-commit-builder { isHook = false; }; };

          RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
        }; # End of `buildNickel` implementation

      buildNickelWasm =
        { channel ? "stable"
        , optimize ? true
        }:
        let
          rust = mkRust {
            inherit channel;
            target = "wasm32-unknown-unknown";
          };
        in
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
            nodePackages.typescript
            # Required by `keytar`, which is a dependency of `vsce`.
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
        buildWasm = buildNickelWasm { optimize = true; };
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
          value = buildNickel { inherit channel; isDevShell = true; };
        }
        ));

      checks = {
        # wasm-opt can take long: eschew optimizations in checks
        wasm = buildNickelWasm { channel = "stable"; optimize = false; };
        pre-commit = packages.default.pre-commit-inner;
      } // (forEachRustChannel (channel:
        {
          name = "nickel-against-${channel}-rust-channel";
          value = buildNickel { inherit channel; };
        }
      ));
    }
    );
}
