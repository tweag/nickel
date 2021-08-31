{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  # We need a fixed version for wasm-bindgen-cli, that corresponds to
  # Cargo.toml's one
  inputs.nixpkgs-pinned.url = "nixpkgs/nixos-21.05";
  inputs.nixpkgs-mozilla.url = "github:nickel-lang/nixpkgs-mozilla/flake";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  outputs = { self, nixpkgs, nixpkgs-pinned, nixpkgs-mozilla, import-cargo }:
    let

      SYSTEMS = [
        "x86_64-linux"
        "x86_64-darwin"
      ];

      RUST_CHANNELS = readRustChannels [
        "stable"
        "beta"
        "nightly"
      ];

      inherit (nixpkgs.lib) genAttrs substring;

      version = "${substring 0 8 self.lastModifiedDate}-${self.shortRev or "dirty"}";

      readRustChannel = c: builtins.fromTOML (builtins.readFile (./. + "/scripts/channel_${c}.toml"));
      readRustChannels = cs: builtins.listToAttrs (map (c: { name = c; value = readRustChannel c; }) cs);

      forAllSystems = f: genAttrs SYSTEMS (system: f system);

      # Instantiate nixpkgs with the mozilla Rust overlay
      mkPkgs = { system }:
        import nixpkgs {
          inherit system;
          overlays = [ nixpkgs-mozilla.overlays.rust ];
        };

      mkCargoHome = { pkgs }:
        (import-cargo.builders.importCargo {
          lockFile = ./Cargo.lock;
          inherit pkgs;
        }).cargoHome;

      # Additional packages required for some systems to build Nickel
      missingSysPkgs = { system, pkgs }:
        if system == "x86_64-darwin" then
          [
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.libiconv
          ]
        else
          [];

      buildNickel = { system, isShell ? false, channel ? "stable" }:
        let
          pkgs = mkPkgs {inherit system;};

          rust =
            (pkgs.rustChannelOf RUST_CHANNELS."${channel}").rust.override({
               extensions = if isShell then [
                "rust-src"
                "rust-analysis"
                "rustfmt-preview"
                "clippy-preview"
              ] else [];
            });

          cargoHome = mkCargoHome {inherit pkgs;};

        in pkgs.stdenv.mkDerivation {
          name = "nickel-${version}";

          buildInputs = [ rust ]
            ++ missingSysPkgs {inherit system pkgs;} ++ (
            if isShell then
              [ pkgs.nodePackages.makam ]
              # TMP for debu
              ++ [ pkgs.wasm-pack pkgs.wasm-bindgen-cli ]
            else
              [ cargoHome ]
          );

          src = if isShell then null else self;

          buildPhase = "cargo build --release --frozen --offline";

          doCheck = true;

          checkPhase =
            ''
              cargo test --release --frozen --offline
              cargo fmt --all -- --check
            '';

          installPhase =
            ''
              mkdir -p $out
              cargo install --frozen --offline --path . --root $out
              rm $out/.crates.toml
            '';
        };


      buildNickelWASM = { system, channel ? "stable" }:
        let
          pkgs = mkPkgs {inherit system;};
          pkgsPinned = import nixpkgs-pinned {inherit system;};

          rust = (pkgs.rustChannelOf RUST_CHANNELS."${channel}").rust.override({
            targets = ["wasm32-unknown-unknown"];
          });

          cargoHome = mkCargoHome {inherit pkgs;};

        in pkgs.stdenv.mkDerivation {
          name = "nickel-wasm-${version}";

          buildInputs =
            [
              rust
              pkgs.wasm-pack
              pkgsPinned.wasm-bindgen-cli
              # Useless for now, as wasm-pack doesn't want to pick our wasm-opt
              # version. See https://github.com/rustwasm/wasm-pack/issues/869
              # pkgs.binaryen
              cargoHome
            ]
            ++ missingSysPkgs {inherit system pkgs;};

          nativeBuildInputs = [pkgs.jq];

          src = self;

          preBuild =
            ''
              # Wasm-pack requires to change the crate type. Cargo doesn't yet
              # support having different crate types depending on the target, so
              # we switch there
              # printf '\n[lib]\ncrate-type = ["cdylib", "rlib"]' >> Cargo.toml
            '';

          buildPhase =
            ''
              printf '\n[lib]\ncrate-type = ["cdylib", "rlib"]' >> Cargo.toml
              # Because of wasm-pack not using existing wasm-opt
              # (https://github.com/rustwasm/wasm-pack/issues/869), we have to
              # break hermeticity and allow network install for now
              # wasm-pack build --mode no-install -- --no-default-features --features repl-wasm --frozen --offline
              wasm-pack build -- --no-default-features --features repl-wasm
            '';

          postBuild = ''
            # Wasm-pack forces the name of both the normal crate and the
            # generated NPM package to be the same. Unfortunately, there already
            # exists a nickel package in the NPM registry, so we use nickel-repl
            # instead
            cd pkg
            jq '.name = "nickel-repl"' package.json > package.json.patched \
              && rm -f ./pkg/package.json \
              && mv package.json.patched package.json
          '';

          installPhase =
            ''
              mkdir -p $out
              cp -r pkg/* $out/nickel-repl
            '';
        };


      buildDocker = { system }:
        let
          pkgs = import nixpkgs { inherit system; };
          nickel = buildNickel { inherit system; };
        in pkgs.dockerTools.buildLayeredImage {
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


      buildMakamSpecs = { system }:
        let
          pkgs = import nixpkgs { inherit system; };
        in pkgs.stdenv.mkDerivation {
          name = "nickel-makam-specs-${version}";
          src = ./makam-spec/src;
          buildInputs =
            [ pkgs.nodePackages.makam
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

    in rec {
      defaultPackage = forAllSystems (system: packages."${system}".build);
      devShell = forAllSystems (system: buildNickel { inherit system; isShell = true; });
      packages = forAllSystems (system: {
        build = buildNickel { inherit system; };
        buildWasm = buildNickelWASM { inherit system; };
        dockerImage = buildDocker { inherit system; };
      });

      checks = forAllSystems (system:
        { specs = buildMakamSpecs { inherit system; };
        } //
        (builtins.listToAttrs
          (map (channel:
            { name = "nickel-against-${channel}-rust-channel";
              value = buildNickel { inherit system channel; };
            }
          )
          (builtins.attrNames RUST_CHANNELS))
        )
      );
    };
}
