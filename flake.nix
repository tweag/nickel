{
  inputs.nixpkgs.url = "nixpkgs/nixos-20.09";
  inputs.nixpkgs-mozilla.url = "github:garbas/nixpkgs-mozilla/flake";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  outputs = { self, nixpkgs, nixpkgs-mozilla, import-cargo }:
    let

      CHANNELS = {
        nightly =
          { channel = "nightly";
            date = "2021-01-15";
            sha256 = "sha256-uoGBMgGmIPj4E+jCY8XH41Ia8NbaRjDC3KioiaCA/M8=";
          };
        beta =
          { channel = "beta";
            date = "2021-01-15";
            sha256 = "sha256-DE2l5X02z9o4z8E5bmBG4QXWOAKEStdVmjZdGZE7PAQ=";
          };
        stable =
          { channel = "stable";
            date = "2020-12-31";
            sha256 = "sha256-KCh2UBGtdlBJ/4UOqZlxUtcyefv7MH1neoVNV4z0nWs=";
          };
      };

      SYSTEMS = [
        "x86_64-linux"
        "x86_64-darwin"
      ];

      inherit (nixpkgs.lib) genAttrs substring;

      forAllSystems = f: genAttrs SYSTEMS (system: f system);

      buildPackage = { system, isShell ? false, channel ? "stable" }:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ nixpkgs-mozilla.overlays.rust ];
          };

          rust =
            (pkgs.rustChannelOf CHANNELS."${channel}").rust.override({
              # targets = [];
              # extentions = [];
            });

        in pkgs.stdenv.mkDerivation {
          name = "nickel-${substring 0 8 self.lastModifiedDate}-${self.shortRev or "dirty"}";

          buildInputs =
            [ rust
            ] ++ (if isShell then [] else [
              (import-cargo.builders.importCargo {
                lockFile = ./Cargo.lock;
                inherit pkgs;
              }).cargoHome
            ]);

          src = if isShell then null else self;

          buildPhase = "cargo build --release --frozen --offline";

          doCheck = true;

          checkPhase = "cargo test --release --frozen --offline";

          installPhase =
            ''
              mkdir -p $out
              cargo install --frozen --offline --path . --root $out
              rm $out/.crates.toml
            '';
        };

    in {
      defaultPackage = forAllSystems (system: buildPackage { inherit system; });
      devShell = forAllSystems (system: buildPackage { inherit system; isShell = true; });
      checks =
        forAllSystems (system:
          builtins.listToAttrs
            (map (channel:
              { name = "build_${channel}";
                value = buildPackage { inherit system channel; };
              }
            )
            (builtins.attrNames CHANNELS))
        );
    };
}
