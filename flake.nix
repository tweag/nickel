{
  inputs.nixpkgs.url = "nixpkgs/nixos-20.09";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  outputs = { self, nixpkgs, import-cargo }:
    with import nixpkgs { system = "x86_64-linux"; };
    with pkgs;

    let

      buildPackage = { isShell }: stdenv.mkDerivation {
        name = "nickel-${lib.substring 0 8 self.lastModifiedDate}-${self.shortRev or "dirty"}";

        buildInputs =
          [ rustc
            cargo
          ] ++ (if isShell then [
            rustfmt
            clippy
          ] else [
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

      defaultPackage.x86_64-linux = buildPackage { isShell = false; };

      checks.x86_64-linux.build = self.defaultPackage.x86_64-linux;

      devShell.x86_64-linux = buildPackage { isShell = true; };

    };
}
