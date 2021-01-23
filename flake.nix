{
  inputs.nixpkgs.url = "nixpkgs/nixos-20.09";
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

      readRustChannel = c: builtins.fromTOML (builtins.readFile (./. + "/scripts/channel_${c}.toml"));
      readRustChannels = cs: builtins.listToAttrs (map (c: { name = c; value = readRustChannel c; }) cs);

      forAllSystems = f: genAttrs SYSTEMS (system: f system);

      buildPackage = { system, isShell ? false, channel ? "stable" }:
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
            (builtins.attrNames RUST_CHANNELS))
        );
    };
}
