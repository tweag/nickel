{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    topiary = {
      url = "github:tweag/topiary";
      inputs.nixpkgs.follows = "nixpkgs";
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
    , topiary
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
        ];
      };

      wasm-bindgen-cli =
        let
          wasmBindgenCargoVersions = builtins.map ({ version, ... }: version) (builtins.filter ({ name, ... }: name == "wasm-bindgen") cargoLock.package);
          wasmBindgenVersion = assert builtins.length wasmBindgenCargoVersions == 1; builtins.elemAt wasmBindgenCargoVersions 0;
        in
        pkgs.wasm-bindgen-cli.override {
          version = wasmBindgenVersion;
          hash = "sha256-0rK+Yx4/Jy44Fw5VwJ3tG243ZsyOIBBehYU54XP/JGk=";
          cargoHash = "sha256-vcpxcRlW1OKoD64owFF6mkxSqmNrvY+y3Ckn5UwEQ50=";
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
          inherit (pkgs.rust) toRustTarget;
        in
        { rustProfile ? "minimal"
        , rustExtensions ? [
            "rust-src"
            "rust-analysis"
            "rustfmt"
            "clippy"
          ]
        , channel ? "stable"
        , targets ? [ (toRustTarget hostPlatform) ]
            ++ pkgs.lib.optional (!hostPlatform.isMacOS) (toRustTarget pkgs.pkgsMusl.stdenv.hostPlatform)
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

          # we could use pre-commit-hook's built-in topiary, be for now, Topiary
          # is evolving quickly and we prefer to have the latest version.
          # This might change once the Nickel support is stabilized.
          topiary-latest = topiary.lib.${system}.pre-commit-hook // {
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
          importsFilter = mkFilter ".*/core/tests/integration/imports/imported/.*$"; # include all files that are imported in tests
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
              filterCargoSources
              importsFilter
            ];
        };

      # Given a rust toolchain, provide Nickel's Rust dependencies, Nickel, as
      # well as rust tools (like clippy)
      mkCraneArtifacts = { rust ? mkRust { }, noRunBench ? false }:
        let
          craneLib = crane.lib.${system}.overrideToolchain rust;

          # suffixes get added via pnameSuffix
          pname = "nickel-lang";

          # Customize source filtering as Nickel uses non-standard-Rust files like `*.lalrpop`.
          src = filterNickelSrc craneLib.filterCargoSources;

          # set of cargo args common to all builds
          cargoBuildExtraArgs = "--frozen --offline";

          # Build *just* the cargo dependencies, so we can reuse all of that work (e.g. via cachix) when running in CI
          cargoArtifacts = craneLib.buildDepsOnly {
            inherit pname src;
            cargoExtraArgs = "${cargoBuildExtraArgs} --workspace --all-features";
            # pyo3 needs a Python interpreter in the build environment
            # https://pyo3.rs/v0.17.3/building_and_distribution#configuring-the-python-version
            buildInputs = [ pkgs.python3 ];
          };

          env = {
            NICKEL_NIX_BUILD_REV = self.shortRev or "dirty";
          };

          buildPackage = { pname, extraBuildArgs ? "", extraArgs ? { } }:
            craneLib.buildPackage ({
              inherit
                pname
                src
                version
                cargoArtifacts
                env;

              cargoExtraArgs = "${cargoBuildExtraArgs} ${extraBuildArgs} --package ${pname}";
            } // extraArgs);
        in
        rec {
          inherit cargoArtifacts;
          nickel-lang-core = buildPackage { pname = "nickel-lang-core"; };
          nickel-lang-cli = buildPackage { pname = "nickel-lang-cli"; };
          lsp-nls = buildPackage { pname = "nickel-lang-lsp"; };

          # Static building isn't really possible on MacOS because the system call ABIs aren't stable.
          nickel-static =
            if pkgs.stdenv.hostPlatform.isMacOS
            then nickel-lang-cli
            else
            # To build Nickel and its dependencies statically we use the musl
            # libc and clang with libc++ to build C and C++ dependencies. We
            # tried building with libstdc++ but without success.
              buildPackage {
                pname = "nickel-lang-cli-static";
                extraArgs = {
                  CARGO_BUILD_TARGET = pkgs.rust.toRustTarget pkgs.pkgsMusl.stdenv.hostPlatform;
                  # For some reason, the rust build doesn't pick up the paths
                  # to `libcxx` and `libcxxabi` from the stdenv. So we specify
                  # them explicitly. Also, `libcxx` expects to be linked with
                  # `libcxxabi` at the end, and we need to make the rust linker
                  # aware of that.
                  RUSTFLAGS = "-L${pkgs.pkgsMusl.llvmPackages.libcxx}/lib -L${pkgs.pkgsMusl.llvmPackages.libcxxabi}/lib -lstatic=c++abi";
                  # Explain to `cc-rs` that it should use the `libcxx` C++
                  # standard library, and a static version of it, when building
                  # C++ libraries. The `cc-rs` crate is typically used in
                  # upstream build.rs scripts.
                  CXXSTDLIB = "static=c++";
                  stdenv = pkgs.pkgsMusl.libcxxStdenv;
                  doCheck = false;
                };
              };

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
            inherit (cargoArtifacts) buildInputs;

            RUSTDOCFLAGS = "-D warnings";

            cargoExtraArgs = "${cargoBuildExtraArgs} --workspace --all-features";

            doInstallCargoArtifacts = false;
          };

          rustfmt = craneLib.cargoFmt {
            # Notice that unlike other Crane derivations, we do not pass `cargoArtifacts` to `cargoFmt`, because it does not need access to dependencies to format the code.
            inherit pname src env;

            cargoExtraArgs = "--all";

            # `-- --check` is automatically prepended by Crane
            rustFmtExtraArgs = "--color always";
          };

          clippy = craneLib.cargoClippy {
            inherit pname src cargoArtifacts env;
            inherit (cargoArtifacts) buildInputs;

            cargoExtraArgs = cargoBuildExtraArgs;
            cargoClippyExtraArgs = "--all-features --all-targets --workspace -- --deny warnings --allow clippy::new-without-default --allow clippy::match_like_matches_macro";
          };
        };

      makeDevShell = { rust }: pkgs.mkShell {
        # Trick found in Crane's examples to get a nice dev shell
        # See https://github.com/ipetkov/crane/blob/master/examples/quick-start/flake.nix
        inputsFrom = builtins.attrValues (mkCraneArtifacts { inherit rust; });

        buildInputs = [
          pkgs.rust-analyzer
          pkgs.cargo-insta
          pkgs.nixpkgs-fmt
          pkgs.nodejs
          pkgs.yarn
          pkgs.yarn2nix
          pkgs.nodePackages.markdownlint-cli
          pkgs.python3
          topiary.packages.${system}.default
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
          craneLib = crane.lib.${system}.overrideToolchain rust;

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
          Entrypoint = "${nickel}/bin/nickel";
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
              ${self.packages."${system}".default}/bin/nickel doc --format "${format}" -f "$module.ncl" \
                --output "$out/$module.${extension}"
            done
          '';
        };

    in
    rec {
      packages = {
        inherit (mkCraneArtifacts { })
          nickel-lang-cli
          benchmarks
          lsp-nls
          cargoArtifacts;
        default = pkgs.buildEnv {
          name = "nickel";
          paths = [ packages.nickel-lang-cli packages.lsp-nls ];
        };
        nickelWasm = buildNickelWasm { };
        dockerImage = buildDocker packages.nickel-lang-cli; # TODO: docker image should be a passthru
        inherit vscodeExtension;
        inherit userManual;
        stdlibMarkdown = stdlibDoc "markdown";
        stdlibJson = stdlibDoc "json";
      } // pkgs.lib.optionalAttrs (!pkgs.stdenv.hostPlatform.isDarwin) {
        inherit (mkCraneArtifacts { }) nickel-static;
        # Use the statically linked binary for the docker image if we're not on MacOS.
        dockerImage = buildDocker packages.nickel-static;
      };

      apps = {
        default = {
          type = "app";
          program = "${packages.nickel-lang-cli}/bin/nickel";
        };
      };

      devShells = (forEachRustChannel (channel: {
        name = channel;
        value = makeDevShell { rust = mkRust { inherit channel; rustProfile = "default"; targets = [ "wasm32-unknown-unknown" ]; }; };
      })) // {
        default = devShells.stable;
      };

      checks = {
        inherit (mkCraneArtifacts { noRunBench = true; })
          benchmarks
          clippy
          checkRustDoc
          lsp-nls
          nickel-lang-cli
          nickel-lang-core
          rustfmt;
        # An optimizing release build is long: eschew optimizations in checks by
        # building a dev profile
        nickelWasm = buildNickelWasm { profile = "dev"; };
        inherit vscodeExtension;
        pre-commit = pre-commit-builder { };
      };
    }
    );
}
