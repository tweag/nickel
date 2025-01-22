{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
    };
    nix-input = {
      url = "github:nixos/nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "pre-commit-hooks/flake-compat";
      };
    };
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
    , rust-overlay
    , crane
    , nix-input
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
      ];

      forEachRustChannel = fn: builtins.listToAttrs (builtins.map fn RUST_CHANNELS);

      cargoTOML = builtins.fromTOML (builtins.readFile ./Cargo.toml);
      cargoLock = builtins.fromTOML (builtins.readFile ./Cargo.lock);

      inherit (cargoTOML.workspace.package) version;

    in
    flake-utils.lib.eachSystem SYSTEMS (system:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import rust-overlay)
          # gnulib tests in diffutils fail for musl arm64, cf. https://github.com/NixOS/nixpkgs/pull/241281
          (final: prev: {
            diffutils =
              if !(final.stdenv.hostPlatform.isMusl && final.stdenv.hostPlatform.isAarch64) then
                prev.diffutils
              else
                prev.diffutils.overrideAttrs (old: {
                  postPatch = ''
                    sed -i 's:gnulib-tests::g' Makefile.in
                  '';
                });
          })
        ];
        config.allowUnfreePredicate = pkg: builtins.elem (pkg.pname or "") [ "terraform" ];
      };

      wasm-bindgen-cli =
        let
          wasmBindgenCargoVersions = builtins.map ({ version, ... }: version) (builtins.filter ({ name, ... }: name == "wasm-bindgen") cargoLock.package);
          wasmBindgenVersion = assert builtins.length wasmBindgenCargoVersions == 1; builtins.elemAt wasmBindgenCargoVersions 0;
        in
        pkgs.wasm-bindgen-cli.override {
          version = wasmBindgenVersion;
          hash = "sha256-f/RK6s12ItqKJWJlA2WtOXtwX4Y0qa8bq/JHlLTAS3c=";
          cargoHash = "sha256-3vxVI0BhNz/9m59b+P2YEIrwGwlp7K3pyPKt4VqQuHE=";
        };

      # Additional packages required to build Nickel on Darwin
      systemSpecificPkgs =
        if pkgs.stdenv.isDarwin then
          [
            pkgs.darwin.apple_sdk.frameworks.Security
            pkgs.darwin.libiconv
          ]
        else
          [ ];

      mkRust =
        let
          inherit (pkgs.stdenv) hostPlatform;
        in
        { rustProfile ? "minimal"
        , rustExtensions ? [
            "rust-src"
            "rust-analysis"
            "rustfmt"
            "clippy"
          ]
        , channel ? "stable"
        , targets ? [ pkgs.stdenv.hostPlatform.rust.rustcTarget ]
            ++ pkgs.lib.optional (!hostPlatform.isMacOS) pkgs.pkgsMusl.stdenv.hostPlatform.rust.rustcTarget
        }:
        if channel == "nightly" then
          pkgs.rust-bin.selectLatestNightlyWith
            (toolchain: toolchain.${rustProfile}.override {
              extensions = rustExtensions;
              inherit targets;
            })
        else
          pkgs.rust-bin.${channel}.latest.${rustProfile}.override {
            extensions = rustExtensions;
            inherit targets;
          };

      # A note on check_format: the way we invoke rustfmt here works locally but fails on CI.
      # Since the formatting is checked on CI anyway - as part of the rustfmt check - we
      # disable rustfmt in the pre-commit hook when running checks, but enable it when
      # running in a dev shell.
      pre-commit-builder = { rust ? mkRust { }, checkFormat ? false }: pre-commit-hooks.lib.${system}.run {
        src = self;
        hooks = {
          nixpkgs-fmt = {
            enable = true;
            # Excluded because they are generated by Node2nix
            excludes = [
              "lsp/vscode-extension/default.nix"
              "lsp/vscode-extension/node-env.nix"
              "lsp/vscode-extension/node-packages.nix"
            ];
          };

          rustfmt = {
            enable = checkFormat;
            entry = pkgs.lib.mkForce "${rust}/bin/cargo-fmt fmt -- --check --color always";
          };

          markdownlint = {
            enable = true;
            excludes = [
              "notes/(.+)\\.md$"
              "^RELEASES\\.md$"
            ];
          };

          # We could use Topiary here, but the Topiary version pulled from Nix
          # and the one baked in Nickel could differ. It's saner that what we
          # check in the CI is matching exactly the formatting performed by the
          # `nickel` binary of this repo.
          nickel-format = {
            name = "nickel-format";
            description = "The nickel formatter";
            entry = "${pkgs.lib.getExe self.packages."${system}".default} format";
            types = [ "text" ];
            enable = true;
            # Some tests are currently failing the idempotency check, and
            # formatting is less important there. We at least want the examples
            # as well as the stdlib to be properly formatted.
            files = "\\.ncl$";
            excludes = [
              "/tests/(.+)\\.ncl$"
            ];
          };
        };
      };

      # Customize source filtering for Crane as Nickel uses non-standard-Rust
      # files like `*.lalrpop`.
      filterNickelSrc = filterCargoSources:
        let
          mkFilter = regexp: path: _type: builtins.match regexp path != null;
          lalrpopFilter = mkFilter ".*lalrpop$";
          nclFilter = mkFilter ".*ncl$";
          txtFilter = mkFilter ".*txt$";
          snapFilter = mkFilter ".*snap$";
          scmFilter = mkFilter ".*scm$";
          mdFilter = mkFilter ".*md$"; # include markdown files for checking snippets in the documentation
          cxxFilter = mkFilter ".*(cc|hh)$";
          importsFilter = mkFilter ".*/core/tests/integration/inputs/imports/imported/.*$"; # include all files that are imported in tests

          infraFilter = mkFilter ".*/infra/.*$";
        in
        pkgs.lib.cleanSourceWith {
          src = pkgs.lib.cleanSource ./.;

          # Combine our custom filters with the default one from Crane
          # See https://github.com/ipetkov/crane/blob/master/docs/API.md#libfiltercargosources
          filter = path: type:
            builtins.any (filter: filter path type) [
              lalrpopFilter
              nclFilter
              txtFilter
              snapFilter
              scmFilter
              mdFilter
              cxxFilter
              filterCargoSources
              importsFilter
            ] && !(builtins.any (filter: filter path type) [
              infraFilter
            ]);
        };

      # if we directly set the revision, it would invalidate the cache on every commit.
      # instead we set a static dummy hash and edit the binary in a separate (fast) derivation.
      dummyRev = "DUMMYREV_THIS_SHOULD_NOT_APPEAR_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

      # pad a string with the tail of another string
      padWith = pad: str:
        str +
        builtins.substring
          (builtins.stringLength str)
          (builtins.stringLength pad)
          pad;

      # We want `nickel --version` and `nls --version` to print the git revision
      # that nickel was compiled from. However, putting self.shortRev in a
      # derivation invalidates the cache on any change, even if otherwise the
      # derivation is identical. To mitigate this, we pass an unchanging string
      # as the revision in `NICKEL_NIX_BUILD_REV`, and then have a small wrapper
      # that replaces that string in the output binary. On every new commit this
      # fast derivation will have to be rebuilt, but the slow compilation of
      # rust code will only happen on more substantial changes. This is only
      # needed for binaries that actually make use of this information (the
      # cli and the language server).
      fixupGitRevision = pkg: pkgs.stdenv.mkDerivation {
        pname = pkg.pname + "-rev-fixup";
        inherit (pkg) version meta;
        src = pkg;
        buildInputs = [ pkgs.bbe ]
          ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.autoSignDarwinBinariesHook ];
        phases = [ "fixupPhase" ];
        fixupPhase = ''
          runHook preFixup

          mkdir -p $out/bin
          for srcBin in $src/bin/*; do
            outBin="$out/bin/$(basename $srcBin)"
            # [dirty] must have 7 characters to match dummyRev (hard coded in
            # nickel-lang-cli and nickel-lang-lsp)
            # we have to pad them out to the same length as dummyRev so they fit
            # in the same spot in the binary
            bbe -e 's/${dummyRev}/${padWith dummyRev (self.shortRev or "[dirty]")}/' \
              $srcBin > $outBin
            chmod +x $outBin
          done

          runHook postFixup
        '';
      };

      # `crane.lib.${system}` is now deprecated, we must use
      # `(crane.mkLib nixpkgs.legacyPackages.${system})` instead. Since we
      # only ever use `crane.lib.${system}.overrideToolchain` in this flake, we
      # expose that function as a top-level function`.
      craneOverrideToolchain =
        (crane.mkLib pkgs).overrideToolchain;

      # Given a rust toolchain, provide Nickel's Rust dependencies, Nickel, as
      # well as rust tools (like clippy)
      mkCraneArtifacts = { rust ? mkRust { }, noRunBench ? false }:
        let
          craneLib = craneOverrideToolchain rust;

          # suffixes get added via pnameSuffix
          pname = "nickel-lang";

          # Customize source filtering as Nickel uses non-standard-Rust files like `*.lalrpop`.
          src = filterNickelSrc craneLib.filterCargoSources;

          # set of cargo args common to all builds
          cargoBuildExtraArgs = "--frozen --offline";

          # Build *just* the cargo dependencies, so we can reuse all of that work (e.g. via cachix) when running in CI
          cargoArtifactsDeps = craneLib.buildDepsOnly {
            inherit pname src;
            cargoExtraArgs = "${cargoBuildExtraArgs} --all-features";
            # If we build all the packages at once, feature unification takes
            # over and we get libraries with different sets of features than
            # we would get building them separately. Meaning that when we
            # later build them separately, it won't hit the cache. So instead,
            # we need to build each package separately when we are collecting
            # dependencies.
            cargoBuildCommand = "cargoWorkspace build";
            cargoTestCommand = "cargoWorkspace test";
            cargoCheckCommand = "cargoWorkspace check";
            preBuild = ''
              cargoWorkspace() {
                command=$(shift)
                for packageDir in $(${pkgs.yq}/bin/tomlq -r '.workspace.members[]' Cargo.toml); do
                  (
                    cd $packageDir
                    pwd
                    cargoWithProfile $command "$@"
                  )
                done
              }
            '';
            # pyo3 needs a Python interpreter in the build environment
            # https://pyo3.rs/v0.17.3/building_and_distribution#configuring-the-python-version
            nativeBuildInputs = with pkgs; [ pkg-config python3 ];
            buildInputs =
              # SEE: https://github.com/NixOS/nix/issues/9107
              let
                disableChecksOnDarwin =
                  pkgList: builtins.map
                    (pkg: pkg.overrideAttrs (_: pkgs.lib.optionalAttrs (system == "x86_64-darwin") {
                      doCheck = false;
                    }))
                    pkgList;
              in

              disableChecksOnDarwin [
                nix-input.packages.${system}.nix
                # When updating to latest Nix, we'll need to use the following
                # additional output. For now, we pinned `nix-input` to a
                # previous tag, where the outputs are still grouped in the
                # default package, so we leave them commented out.
                # nix-input.packages.${system}.nix-store
                # nix-input.packages.${system}.nix-expr
                # nix-input.packages.${system}.nix-flake
                # nix-input.packages.${system}.nix-cmd
              ]
              ++ [
                pkgs.boost # implicit dependency of nix
              ];

            # seems to be needed for consumer cargoArtifacts to be able to use
            # zstd mode properly
            installCargoArtifactsMode = "use-zstd";
          };

          env = {
            NICKEL_NIX_BUILD_REV = dummyRev;
          };

          buildPackage = { pnameSuffix, cargoPackage ? "${pname}${pnameSuffix}", extraBuildArgs ? "", extraArgs ? { } }:
            craneLib.buildPackage ({
              inherit
                pname
                pnameSuffix
                src
                version
                cargoArtifacts;

              cargoExtraArgs = "${cargoBuildExtraArgs} ${extraBuildArgs} --package ${cargoPackage}";
            } // extraArgs);

          # To build Nickel and its dependencies statically we use the musl
          # libc and clang with libc++ to build C and C++ dependencies. We
          # tried building with libstdc++ but without success.
          buildStaticPackage = { pnameSuffix, cargoPackage, extraBuildArgs ? "", extraArgs ? { } }:
            (buildPackage {
              inherit pnameSuffix cargoPackage;
              extraArgs = {
                inherit env;
                CARGO_BUILD_TARGET = pkgs.pkgsMusl.stdenv.hostPlatform.rust.rustcTarget;
                # For some reason, the rust build doesn't pick up the paths
                # to `libcxx`. So we specify them explicitly.
                #
                # We also explicitly add `libc` because of
                # https://github.com/rust-lang/rust/issues/89626.
                RUSTFLAGS = "-L${pkgs.pkgsMusl.llvmPackages.libcxx}/lib -lstatic=c++abi -C link-arg=-lc";
                # Explain to `cc-rs` that it should use the `libcxx` C++
                # standard library, and a static version of it, when building
                # C++ libraries. The `cc-rs` crate is typically used in
                # upstream build.rs scripts.
                CXXSTDLIB = "static=c++";
                stdenv = pkgs.pkgsMusl.libcxxStdenv;
                doCheck = false;
              } // extraArgs;
            });

          # In addition to external dependencies, we build the lalrpop file in a
          # separate derivation because it's expensive to build but needs to be
          # rebuilt infrequently.
          cargoArtifacts = buildPackage {
            pnameSuffix = "-core-lalrpop";
            cargoPackage = "${pname}-core";
            extraArgs = {
              cargoArtifacts = cargoArtifactsDeps;
              src = craneLib.mkDummySrc {
                inherit src;

                # after stubbing out, reset things back just enough for lalrpop build
                extraDummyScript = ''
                  mkdir -p $out/core/src/parser
                  cp ${./core/build.rs} $out/core/build.rs
                  cp ${./core/src/parser/grammar.lalrpop} $out/core/src/parser/grammar.lalrpop
                  # package.build gets set to a dummy file. reset it to use local build.rs
                  # tomlq -i broken (https://github.com/kislyuk/yq/issues/130 not in nixpkgs yet)
                  ${pkgs.yq}/bin/tomlq -t 'del(.package.build)' $out/core/Cargo.toml > tmp
                  mv tmp $out/core/Cargo.toml
                '';
              };
              # the point of this is to cache lalrpop compilation
              doInstallCargoArtifacts = true;
              # we need the target/ directory to be writable
              installCargoArtifactsMode = "use-zstd";
            };
          };
        in
        rec {
          inherit cargoArtifacts cargoArtifactsDeps;
          nickel-lang-core = buildPackage { pnameSuffix = "-core"; };
          nickel-lang-cli = fixupGitRevision (buildPackage {
            pnameSuffix = "-cli";
            extraArgs = {
              inherit env;
              meta.mainProgram = "nickel";
            };
          });
          nickel-lang-lsp = fixupGitRevision (buildPackage {
            pnameSuffix = "-lsp";
            extraArgs = {
              inherit env;
              meta.mainProgram = "nls";
            };
          });

          # Static building isn't really possible on MacOS because the system call ABIs aren't stable.
          nickel-static =
            if pkgs.stdenv.hostPlatform.isMacOS
            then nickel-lang-cli
            else
              fixupGitRevision (buildStaticPackage {
                cargoPackage = "nickel-lang-cli";
                pnameSuffix = "-static";
                extraArgs = { meta.mainProgram = "nickel"; };
              });

          nickel-lang-lsp-static =
            if pkgs.stdenv.hostPlatform.isMacOS
            then nickel-lang-lsp
            else
              fixupGitRevision (buildStaticPackage {
                cargoPackage = "nickel-lang-lsp";
                pnameSuffix = "-static";
                extraArgs = { meta.mainProgram = "nls"; };
              });

          benchmarks = craneLib.mkCargoDerivation {
            inherit pname src version cargoArtifacts env;

            pnameSuffix = "-bench";

            buildPhaseCargoCommand = ''
              cargo bench -p nickel-lang-core ${pkgs.lib.optionalString noRunBench "--no-run"}
            '';

            doInstallCargoArtifacts = false;
          };

          # Check that documentation builds without warnings or errors
          checkRustDoc = craneLib.cargoDoc {
            inherit pname src version cargoArtifacts env;
            inherit (cargoArtifactsDeps) nativeBuildInputs buildInputs;

            RUSTDOCFLAGS = "-D warnings";

            cargoExtraArgs = "${cargoBuildExtraArgs} --workspace --all-features";

            doInstallCargoArtifacts = false;
          };

          rustfmt = craneLib.cargoFmt {
            # Notice that unlike other Crane derivations, we do not pass `cargoArtifacts` to `cargoFmt`, because it does not need access to dependencies to format the code.
            inherit pname src;

            cargoExtraArgs = "--all";

            # `-- --check` is automatically prepended by Crane
            rustFmtExtraArgs = "--color always";
          };

          clippy = craneLib.cargoClippy {
            inherit pname src cargoArtifacts env;
            inherit (cargoArtifactsDeps) nativeBuildInputs buildInputs;

            cargoExtraArgs = cargoBuildExtraArgs;
            cargoClippyExtraArgs = "--all-features --all-targets --workspace -- --deny warnings --allow clippy::new-without-default --allow clippy::match_like_matches_macro";
          };
        };

      makeDevShell = { rust }: pkgs.mkShell {
        # Get deps needed to build. Get them from cargoArtifactsDeps so we build
        # the minimal amount possible to get there. It is a waste of time to
        # build the cargoArtifacts, because cargo won't use them anyways.
        inputsFrom = [ (mkCraneArtifacts { inherit rust; }).cargoArtifactsDeps ];

        buildInputs = [
          pkgs.rust-analyzer
          pkgs.cargo-insta
          pkgs.nixpkgs-fmt
          pkgs.nodejs
          pkgs.yarn
          pkgs.yarn2nix
          pkgs.nodePackages.markdownlint-cli
          pkgs.python3
        ];

        shellHook = (pre-commit-builder { inherit rust; checkFormat = true; }).shellHook + ''
          echo "=== Nickel development shell ==="
          echo "Info: Git hooks can be installed using \`pre-commit install\`"
        '';

        RUST_SRC_PATH = "${rust}/lib/rustlib/src/rust/library";
      };

      # Profile is passed to `wasm-pack`, and is either "dev" (with debug
      # symbols and no optimization), "release" (with optimization and without
      # debug symbols) or "profiling". Right now only dev and release are used:
      #   - release for the production build
      #   - dev for checks, as the code isn't optimized, and WASM optimization
      #   takes time
      buildNickelWasm =
        { rust ? mkRust { targets = [ "wasm32-unknown-unknown" ]; }
        , profile ? "release"
        }:
        let
          # Build the various Crane artifacts (dependencies, packages, rustfmt, clippy) for a given Rust toolchain
          craneLib = craneOverrideToolchain rust;

          # suffixes get added via pnameSuffix
          pname = "nickel-lang-wasm";

          # Customize source filtering as Nickel uses non-standard-Rust files like `*.lalrpop`.
          src = filterNickelSrc craneLib.filterCargoSources;

          cargoExtraArgs = "-p nickel-wasm-repl --target wasm32-unknown-unknown --frozen --offline";
          # *  --mode no-install prevents wasm-pack from trying to download and
          #   vendor tools like wasm-bindgen, wasm-opt, etc. but use the one
          #   provided by Nix
          # * --no-default-features disable some default features of Nickel that
          #   aren't useful for the WASM REPL (and possibly incompatible with
          #   WASM build)
          wasmPackExtraArgs = "--${profile} --mode no-install -- --no-default-features --frozen --offline";

          # Build *just* the cargo dependencies, so we can reuse all of that work (e.g. via cachix) when running in CI
          cargoArtifacts = craneLib.buildDepsOnly {
            inherit pname src cargoExtraArgs;
            doCheck = false;
          };

        in
        craneLib.mkCargoDerivation {
          inherit pname cargoArtifacts src;

          buildPhaseCargoCommand = ''
            WASM_PACK_CACHE=.wasm-pack-cache wasm-pack build wasm-repl ${wasmPackExtraArgs}
          '';

          # nickel-lang.org expects an interface `nickel-repl.wasm`, hence the
          # `ln`
          installPhaseCommand = ''
            mkdir -p $out
            cp -r wasm-repl/pkg $out/nickel-repl
            ln -s $out/nickel-repl/nickel_wasm_repl_bg.wasm $out/nickel-repl/nickel_repl.wasm
          '';

          nativeBuildInputs = [
            rust
            pkgs.wasm-pack
            wasm-bindgen-cli
            pkgs.binaryen
            # Used to include the git revision in the Nickel binary, for `--version`
            pkgs.git
          ] ++ systemSpecificPkgs;
        };

      buildDocker = nickel: pkgs.dockerTools.buildLayeredImage {
        name = "nickel";
        tag = version;
        contents = [
          nickel
        ];
        config = {
          Entrypoint = pkgs.lib.getExe nickel;
          # Labels that are recognized by GHCR
          # See https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-container-registry#labelling-container-images
          Labels = {
            "org.opencontainers.image.source" = "https://github.com/tweag/nickel";
            "org.opencontainers.image.description" = "Nickel: better configuration for less";
            "org.opencontainers.image.licenses" = "MIT";
          };
        };
      };

      # Build the Nickel VSCode extension
      vscodeExtension = pkgs.mkYarnPackage {
        pname = "vscode-nickel";
        src = pkgs.lib.cleanSource ./lsp/vscode-extension;

        buildPhase = ''
          # yarn tries to create a .yarn file in $HOME. There's probably a
          # better way to fix this but setting HOME to TMPDIR works for now.
          export HOME="$TMPDIR"
          cd deps/vscode-nickel
          yarn --offline compile
          yarn --offline vsce package --yarn -o $pname.vsix
        '';

        installPhase = ''
          mkdir $out
          mv $pname.vsix $out
        '';

        distPhase = "true";
      };

      # Copy the markdown user manual to $out.
      userManual = pkgs.stdenv.mkDerivation {
        name = "nickel-user-manual-${version}";
        src = ./doc/manual;
        installPhase = ''
          mkdir -p $out
          cp -r ./ $out
        '';
      };

      # Generate the stdlib documentation from `nickel doc` as `format`.
      stdlibDoc = format:
        let
          extension =
            {
              "markdown" = "md";
            }."${format}" or format;
        in
        pkgs.stdenv.mkDerivation {
          name = "nickel-stdlib-doc-${format}-${version}";
          src = ./core/stdlib;
          installPhase = ''
            mkdir -p $out
            for file in $(ls *.ncl | grep -v 'internals.ncl')
            do
              module=$(basename $file .ncl)
              ${pkgs.lib.getExe self.packages."${system}".default} doc --format "${format}" "$module.ncl" \
                --output "$out/$module.${extension}"
            done
          '';
        };

      infraShell = nickel:
        let
          terraform = pkgs.terraform.withPlugins (p: with p; [
            archive
            aws
            github
          ]);
          ec2-region = "eu-north-1";
          ec2-ami = (import "${nixpkgs}/nixos/modules/virtualisation/amazon-ec2-amis.nix").latest.${ec2-region}.aarch64-linux.hvm-ebs;
          run-terraform = pkgs.writeShellScriptBin "run-terraform" ''
            set -e
            ${pkgs.lib.getExe nickel} export --output main.tf.json <<EOF
              ((import "main.ncl") & {
                region = "${ec2-region}",
                nixos-ami = "${ec2-ami}",
              }).config
            EOF
            ${terraform}/bin/terraform "$@"
          '';

          update-infra = pkgs.writeShellScriptBin "update-infra" ''
            set -e
            ${run-terraform}/bin/run-terraform init
            GITHUB_TOKEN="$(${pkgs.gh}/bin/gh auth token)" ${run-terraform}/bin/run-terraform apply
          '';
        in
        pkgs.mkShell {
          buildInputs = [ terraform run-terraform update-infra ];
        };

      stdlibTests = pkgs.runCommandLocal "stdlib-test" { }
        ''
          ${pkgs.lib.getExe self.packages."${system}".default} test ${./core/stdlib/std.ncl} && mkdir $out
        '';
    in
    rec {
      packages = {
        inherit (mkCraneArtifacts { })
          nickel-lang-core
          nickel-lang-cli
          benchmarks
          nickel-lang-lsp
          cargoArtifacts;
        default = pkgs.buildEnv {
          name = "nickel";
          paths = [ packages.nickel-lang-cli packages.nickel-lang-lsp ];
          meta.mainProgram = "nickel";
        };
        nickelWasm = buildNickelWasm { };
        dockerImage = buildDocker packages.nickel-lang-cli; # TODO: docker image should be a passthru
        inherit vscodeExtension;
        inherit userManual;
        stdlibMarkdown = stdlibDoc "markdown";
        stdlibJson = stdlibDoc "json";
      } // pkgs.lib.optionalAttrs (!pkgs.stdenv.hostPlatform.isDarwin) {
        inherit (mkCraneArtifacts { }) nickel-static nickel-lang-lsp-static;
        # Use the statically linked binary for the docker image if we're not on MacOS.
        dockerImage = buildDocker packages.nickel-static;
      };

      apps = {
        default = {
          type = "app";
          program = pkgs.lib.getExe packages.nickel-lang-cli;
        };
      };

      devShells = (forEachRustChannel (channel: {
        name = channel;
        value = makeDevShell { rust = mkRust { inherit channel; rustProfile = "default"; targets = [ "wasm32-unknown-unknown" ]; }; };
      })) // {
        default = devShells.stable;
        infra = infraShell packages.nickel-lang-cli;
      };

      checks = {
        inherit (mkCraneArtifacts { noRunBench = true; })
          benchmarks
          clippy
          checkRustDoc
          nickel-lang-lsp
          nickel-lang-cli
          nickel-lang-core
          rustfmt;
        # There's a tradeoff here: "release" build is in theory longer than
        # "dev", but it hits the cache on dependencies so in practice it is
        # shorter. Another option would be to compile a dev dependencies version
        # of cargoArtifacts. But that almost doubles the cache space.
        nickelWasm = buildNickelWasm { profile = "release"; };
        inherit vscodeExtension stdlibTests;
        pre-commit = pre-commit-builder { };
      };
    }
    );
}
