{
  inputs.nixpkgs.url = "nixpkgs/nixos-unstable";
  inputs.flake-utils.url  = "github:numtide/flake-utils";
  inputs.rust-overlay.url = "github:oxalica/rust-overlay";
  inputs.import-cargo.url = "github:edolstra/import-cargo";

  nixConfig = {
    extra-substituters = [ "https://nickel.cachix.org" ];
    extra-trusted-public-keys = [ "nickel.cachix.org-1:ABoCOGpTJbAum7U6c+04VbjvLxG9f0gJP5kYihRRdQs=" ];
  };

  outputs = { self
            , nixpkgs
            , flake-utils
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
                rev = version;
                hash = "sha256-GsraYfWzUZjFpPpufTyXF0i2llBzjh04iTKio6m4NRA=";
              };
            in
            final.runCommand "source" { } ''
              cp -R ${tarball} $out
              chmod -R +w $out
              cp ${./wasm-bindgen-api-Cargo.lock} $out/Cargo.lock
            '';
          cargoDeps = old.cargoDeps.overrideAttrs (final.lib.const {
            name = "${old.pname}-${version}-vendor.tar.gz";
            inherit src;
            outputHash = "sha256:0bykharc2iq7r0n51d5rdg9s8dq86r9mc6vvbqlp6i468kp8hdvn";
          });
        });
      };

    in flake-utils.lib.eachSystem SYSTEMS (system:
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
            [];

        buildNickel = { isShell ? false
                      , channel ? "stable"
                      , checkFmt ?  false
                      }:
          let
            rustProfile =
              if isShell then "default"
              else "minimal";

            rust =
              if channel == "nightly" then
                pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.${rustProfile}.override {
                  extensions = [ "rustfmt-preview" ];
                })
              else
                pkgs.rust-bin.${channel}.latest.${rustProfile}.override {
                  extensions = [ "rustfmt-preview" ];
                };

          in pkgs.stdenv.mkDerivation {
            name = "nickel-${version}";

            buildInputs = [ rust ]
              ++ missingSysPkgs ++ (
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
              '' + (if checkFmt then ''

                  cargo fmt --all -- --check
                '' else "");

            installPhase =
              ''
                mkdir -p $out
                cargo install --frozen --offline --path . --root $out
                cargo install --frozen --offline --path lsp/nls --root $out
                rm $out/.crates.toml
              '';

            passhtru.rust = rust;
          };

        buildNickelWASM = { channel ? "stable"
                          , optimize ? true
                          }:
          let
            rust =
              if channel == "nightly" then
                pkgs.rust-bin.selectLatestNightlyWith (toolchain: toolchain.minimal.override {
                  targets = ["wasm32-unknown-unknown"];
                })
              else
                pkgs.rust-bin.${channel}.latest.minimal.override {
                  targets = ["wasm32-unknown-unknown"];
                };
          in pkgs.stdenv.mkDerivation {
            name = "nickel-wasm-${version}";

            src = self;

            nativeBuildInputs = [ pkgs.jq ];

            buildInputs =
              [ rust
                pkgs.wasm-pack
                pkgs.wasm-bindgen-cli
                pkgs.binaryen
                cargoHome
              ] ++ missingSysPkgs;

            preBuild =
              ''
                # Wasm-pack requires to change the crate type. Cargo doesn't yet
                # support having different crate types depending on the target, so
                # we switch there
                sed -i 's/\[lib\]/[lib]\ncrate-type = ["cdylib", "rlib"]/' Cargo.toml

                # This is a hack to prevent the fs2 crate from being compiled on wasm.
                # This may be able to be removed once one or more of these issues are resolved:
                # https://github.com/bheisler/criterion.rs/issues/461
                # https://github.com/rust-lang/cargo/issues/1596
                # https://github.com/rust-lang/cargo/issues/1197
                # https://github.com/rust-lang/cargo/issues/5777
                sed -i '/utilities/d' Cargo.toml
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

        buildDocker = nickel:
          pkgs.dockerTools.buildLayeredImage {
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

        vscodeExtension = 
          let node-package = (pkgs.callPackage ./lsp/client-extension {}).package ;
          in (node-package.override rec {
            pname = "nls-client";
            outputs = [ "vsix" "out" ];
            nativeBuildInputs =  with pkgs; [
              nodePackages.typescript
              # Required by `keytar`, which is a dependency of `vsce`.
              pkg-config libsecret 
            ];
            postInstall = ''
              npm run compile
              mkdir -p $vsix
              echo y | npx vsce package -o $vsix/${pname}.vsix
            '';
          }).vsix;

        buildDevShell = { channel ? "stable" }:
          let
            nickel = buildNickel { isShell = true; };

            rustFormatHook = pkgs.writeShellScriptBin "check-rust-format-hook"
              ''
                ${nickel.rust}/bin/cargo fmt -- --check
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
            packages = [
              (installGitHooks { pre-commit = [ rustFormatHook ]; })
              uninstallGitHooks
            ];
            inputsFrom = [ nickel ];
            shellHook = ''
              echo "=== Nickel development shell ==="
              echo "Info: Git hooks can be installed using \`install-git-hooks\`"
            '';
          };

      in rec {
        packages = {
          build = buildNickel {};
          buildWasm = buildNickelWASM { optimize = true; };
          dockerImage = buildDocker packages.build;
          inherit vscodeExtension;
        };
        defaultPackage = packages.build;

        devShells = 
          builtins.listToAttrs
            (builtins.map (channel: { name = channel;
                                      value = buildDevShell { inherit channel; };
                                    }) RUST_CHANNELS
            );
        devShell = devShells.stable;

        checks = 
          {
            # wasm-opt can take long: eschew optimizations in checks
            wasm = buildNickelWASM { channel = "stable"; optimize = false; };
            specs = makamSpecs;
          } //
          (builtins.listToAttrs
            (builtins.map (channel: let checkFmt = channel == "stable";
                                    in { name = "nickel-against-${channel}-rust-channel";
                                         value = buildNickel { inherit channel checkFmt; };
                                       }) RUST_CHANNELS));
      }
    );
}
