{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.nixpkgs-mozilla.url = "github:garbas/nixpkgs-mozilla/flake";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  outputs = { self, nixpkgs, nixpkgs-mozilla, import-cargo }:
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

      buildNickel = { system, isShell ? false, channel ? "stable" }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ nixpkgs-mozilla.overlays.rust ];
          };

          rust =
            (pkgs.rustChannelOf RUST_CHANNELS."${channel}").rust.override({
              # targets = [];
               extensions = if isShell then [
                "rust-src"
                "rust-analysis"
                "rustfmt-preview"
                "clippy-preview"
              ] else [];

            });

          cargoHome =
            (import-cargo.builders.importCargo {
              lockFile = ./Cargo.lock;
              inherit pkgs;
            }).cargoHome;

        in pkgs.stdenv.mkDerivation {
          name = "nickel-${version}";

          buildInputs =
            [ rust ] ++ (
              if isShell then
                [ pkgs.nodePackages.makam ]
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
