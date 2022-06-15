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
    extra-substituters = [ "https://nickel.cachix.org" ];
    extra-trusted-public-keys = [ "nickel.cachix.org-1:ABoCOGpTJbAum7U6c+04VbjvLxG9f0gJP5kYihRRdQs=" ];
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
      WasmBindgenCargoVersion = cargoTOML.dependencies.wasm-bindgen.version;
      WasmBindgenVersion = builtins.substring 1 (builtins.stringLength WasmBindgenCargoVersion) WasmBindgenCargoVersion;

      version = "${cargoTOML.package.version}_${builtins.substring 0 8 self.lastModifiedDate}_${self.shortRev or "dirty"}";

      customOverlay = final: prev: {
        wasm-bindgen-cli = prev.wasm-bindgen-cli.overrideAttrs (old: rec {
          version = WasmBindgenVersion;
          src =
            let
              tarball = final.fetchFromGitHub {
                owner = "rustwasm";
                repo = "wasm-bindgen";
                rev = WasmBindgenVersion;
                hash = "sha256:041mp2ls78iji4w1v3kka2bbcj0pjwy7svpslk2rhldkymhxmjhs";
              };
            in
            final.runCommand "source" { } ''
              cp -R ${tarball} $out
              chmod -R +w $out
              cp ${./wasm-bindgen-api-Cargo.lock} $out/Cargo.lock
            '';
          checkInputs = [ ];
          cargoTestFlags = [ ];
          cargoBuildFlags = [ "-p" old.pname ];
          cargoDeps = old.cargoDeps.overrideAttrs (final.lib.const {
            name = "${old.pname}-${version}-vendor.tar.gz";
            inherit src;
            outputHash = "sha256:0bykharc2iq7r0n51d5rdg9s8dq86r9mc6vvbqlp6i468kp8hdvn";
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

          pre-commit = pre-commit-hooks.lib.${system}.run {
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
            };
          };

        in
        pkgs.stdenv.mkDerivation {
          name = "nickel-${version}";

          buildInputs =
            [
              rust
            ] ++ missingSysPkgs
            ++ (if isDevShell then [ pkgs.nodePackages.makam ]
            else [ cargoHome ]);

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

          shellHook = pre-commit.shellHook + ''
            echo "=== Nickel development shell ==="
            echo "Info: Git hooks can be installed using \`pre-commit install\`"
          '';

          passthru = { inherit rust pre-commit; };

          RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
        };

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

      makamSpecs = pkgs.stdenv.mkDerivation {
        name = "nickel-makam-specs-${version}";
        src = ./makam-spec/src;
        buildInputs =
          [
            pkgs.nodePackages.makam
          ];
        buildPhase = ''
          # For some reason (bug) the first time I use makam here it doesn't generate any output
          # That's why I'm "building" before testing
          makam init.makam
          makam --run-tests testnickel.makam
        '';
        installPhase = ''
          echo "WORKS" > $out
        '';
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
          cp -r ./ "$out"
        '';
      };

    in
    rec {
      defaultPackage = packages.build;
      packages = {
        build = buildNickel { };
        buildWasm = buildNickelWasm { optimize = true; };
        dockerImage = buildDocker packages.build; # TODO: docker image should be a passthru
        inherit vscodeExtension;
        inherit userManual;
      };

      devShell = devShells.stable;
      devShells = forEachRustChannel
        (channel: {
          name = channel;
          value = buildNickel { inherit channel; isDevShell = true; };
        });

      checks = {
        # wasm-opt can take long: eschew optimizations in checks
        wasm = buildNickelWasm { channel = "stable"; optimize = false; };
        # out of sync, disabling for now -> https://github.com/tweag/nickel/issue/552
        #specs = makamSpecs;
        pre-commit = defaultPackage.pre-commit;
      } // (forEachRustChannel (channel:
        {
          name = "nickel-against-${channel}-rust-channel";
          value = buildNickel { inherit channel; };
        }
      ));
    }
    );
}
