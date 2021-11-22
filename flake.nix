{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  # We need a fixed version for wasm-bindgen-cli, that corresponds to
  # the version of wasm-bindgen in Cargo.toml
  inputs.nixpkgs-wasm.url = "nixpkgs/nixos-21.05";
  inputs.nixpkgs-mozilla.url = "github:nickel-lang/nixpkgs-mozilla/flake";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  nixConfig = {
    substituters = [ "https://nickel.cachix.org" ];
    trusted-public-keys = [ "nickel.cachix.org-1:ABoCOGpTJbAum7U6c+04VbjvLxG9f0gJP5kYihRRdQs=" ];
  };

  outputs = { self, nixpkgs, nixpkgs-wasm, nixpkgs-mozilla, import-cargo }:
    let

      SYSTEMS = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
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

      mkRust = pkgs: channel:
        let
          manifestFile = builtins.fetchurl {
            url = pkgs.lib.rustLib.manifest_v2_url RUST_CHANNELS.${channel};
            sha256 = RUST_CHANNELS.${channel}.sha256;
          };
        in
        pkgs.lib.rustLib.fromManifestFile manifestFile {
          inherit (pkgs) lib stdenv fetchurl patchelf;
        };

      mkCargoHome = { pkgs }:
        (import-cargo.builders.importCargo {
          lockFile = ./Cargo.lock;
          inherit pkgs;
        }).cargoHome;

      # Additional packages required for some systems to build Nickel
      missingSysPkgs = { pkgs }:
        if pkgs.stdenv.isDarwin then
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
            (mkRust pkgs channel).rust.override({
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
            ++ missingSysPkgs {inherit pkgs;} ++ (
            if isShell then
              [ pkgs.nodePackages.makam ]
            else
              [ cargoHome ]
          );

          src = if isShell then null else self;

          buildPhase = "cargo build --workspace --release --frozen --offline";

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
              cargo install --frozen --offline --path lsp/nls --root $out
              rm $out/.crates.toml
            '';
        };


      buildNickelWASM = { system, channel ? "stable", optimize ? true }:
        let
          pkgs = mkPkgs {inherit system;};
          pkgsPinned = import nixpkgs-wasm {inherit system;};

          rust = (mkRust pkgs channel).rust.override({
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
              pkgs.binaryen
              cargoHome
            ]
            ++ missingSysPkgs {inherit pkgs;};

          nativeBuildInputs = [pkgs.jq];

          src = self;

          preBuild =
            ''
              # Wasm-pack requires to change the crate type. Cargo doesn't yet
              # support having different crate types depending on the target, so
              # we switch there
              printf '\n[lib]\ncrate-type = ["cdylib", "rlib"]' >> Cargo.toml
            '';

          buildPhase =
            let optLevel = if optimize then "-O4 " else "-O0";
            in ''
              runHook preBuild

              wasm-pack build --mode no-install -- --no-default-features --features repl-wasm --frozen --offline
              # Because of wasm-pack not using existing wasm-opt
              # (https://github.com/rustwasm/wasm-pack/issues/869), we have to
              # run wasm-opt manually
              echo "[Nix build script]Manually running wasm-opt..."
              wasm-opt ${optLevel} pkg/nickel_bg.wasm -o pkg/nickel_bg.wasm

              runHook postBuild
            '';

          postBuild = ''
            # Wasm-pack forces the name of both the normal crate and the
            # generated NPM package to be the same. Unfortunately, there already
            # exists a nickel package in the NPM registry, so we use nickel-repl
            # instead
            jq '.name = "nickel-repl"' pkg/package.json > package.json.patched \
              && rm -f pkg/package.json \
              && mv package.json.patched pkg/package.json
          '';

          installPhase =
            ''
              mkdir -p $out
              cp -r pkg $out/nickel-repl
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

      buildDevShell = { system, channel ? "stable" }: 
      let 
        pkgs = mkPkgs { inherit system; };
        nickel = buildNickel { inherit system; isShell = true; };
        rust = (mkRust pkgs channel).rust.override({
          extensions = [ "rustfmt-preview" ];
        });
        rustFormatHook = pkgs.writeShellScriptBin "check-rust-format-hook"
          ''
            ${rust}/bin/cargo fmt -- --check
            RESULT=$?
            [ $RESULT != 0 ] && echo "Please run \`cargo fmt\` before"
            exit $RESULT
          '';
        
        installGitHooks = hookTypes:
          let mkHook = type: hooks: {
            hook = pkgs.writeShellScript type
            ''
              for hook in ${pkgs.symlinkJoin { name = "${type}-git-hooks"; paths = hooks; }}/bin/*; do
                $hook
                RESULT=$?
                if [ $RESULT != 0 ]; then
                  echo "$hook returned non-zero: $RESULT, abort operation"
                exit $RESULT
                fi
              done
              echo "$INSTALLED_GIT_HOOKS $type"
              exit 0
            '';
            inherit type;
          };

          installHookScript = { type, hook }: ''
            if [[ -e .git/hooks/${type} ]]; then
                echo "Warn: ${type} hook already present, skipping"
            else
                ln -s ${hook} $PWD/.git/hooks/${type}
                INSTALLED_GIT_HOOKS+=(${type})
            fi
          '';
          in

          pkgs.writeShellScriptBin "install-git-hooks" 
          ''

            if [[ ! -d .git ]] || [[ ! -f flake.nix ]]; then
              echo "Invocate \`nix develop\` from the project root directory."
              exit 1
            fi

            if [[ -e .git/hooks/nix-installed-hooks ]]; then
               echo "Hooks already installed, reinstalling"
               ${uninstallGitHooks.name}
            fi

            mkdir -p ./.git/hooks
            
            ${pkgs.lib.concatStringsSep "\n" (pkgs.lib.mapAttrsToList (type: hooks: installHookScript (mkHook type hooks)) hookTypes )}

            echo "Installed git hooks: $INSTALLED_GIT_HOOKS"
            printf "%s\n" "''${INSTALLED_GIT_HOOKS[@]}" > .git/hooks/nix-installed-hooks
          '';
        
        uninstallGitHooks = pkgs.writeShellScriptBin "uninstall-git-hooks" 
          ''
          if [[ ! -e "$PWD/.git/hooks/nix-installed-hooks" ]]; then
            echo "Error: could find list of installed hooks."
            exit 1
          fi

          while read -r hook
          do
            echo "Uninstalling $hook"
            rm "$PWD/.git/hooks/$hook"
          done < "$PWD/.git/hooks/nix-installed-hooks"

          rm "$PWD/.git/hooks/nix-installed-hooks"
          '';

      in pkgs.mkShell {
        packages = [ (installGitHooks { pre-commit = [rustFormatHook]; } ) uninstallGitHooks ];
        inputsFrom = [ nickel ];

        shellHook = ''
        echo "=== Nickel development shell ==="
        echo "Info: Git hooks can be installed using \`install-git-hooks\`"
        '';
      };

    in rec {
      defaultPackage = forAllSystems (system: packages."${system}".build);
      devShell = forAllSystems (system: buildDevShell { inherit system; });
      packages = forAllSystems (system: {
        build = buildNickel { inherit system; };
        buildWasm = buildNickelWASM { inherit system; optimize = true; };
        dockerImage = buildDocker { inherit system; };
      });

      checks = forAllSystems (system:
        {
          # wasm-opt can take long: eschew optimizations in checks
          wasm = buildNickelWASM { inherit system; channel = "stable"; optimize = false; };
          specs = buildMakamSpecs { inherit system; };
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
