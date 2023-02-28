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

      version = "${cargoTOML.package.version}_${builtins.substring 0 8 self.lastModifiedDate}_${self.shortRev or "dirty"}";

      customOverlay = final: prev: {
        # The version of `wasm-bindgen` CLI *must* be the same as the `wasm-bindgen` Rust dependency in `Cargo.toml`.
        # The definition of `wasm-bindgen-cli` in Nixpkgs does not allow overriding directly the attrset passed to `buildRustPackage`.
        # We instead override the attrset that `buildRustPackage` generates and passes to `mkDerivation`.
        # See https://discourse.nixos.org/t/is-it-possible-to-override-cargosha256-in-buildrustpackage/4393
        wasm-bindgen-cli = prev.wasm-bindgen-cli.overrideAttrs (oldAttrs:
          let
            wasmBindgenCargoVersion = cargoTOML.dependencies.wasm-bindgen.version;
            # Remove the pinning `=` prefix of the version
            wasmBindgenVersion = builtins.substring 1 (builtins.stringLength wasmBindgenCargoVersion) wasmBindgenCargoVersion;
          in
          rec {
            pname = "wasm-bindgen-cli";
            version = wasmBindgenVersion;

            src = final.fetchCrate {
              inherit pname version;
              sha256 = "sha256-+PWxeRL5MkIfJtfN3/DjaDlqRgBgWZMa6dBt1Q+lpd0=";
            };

            cargoDeps = oldAttrs.cargoDeps.overrideAttrs (final.lib.const {
              # This `inherit src` is important, otherwise, the old `src` would be used here
              inherit src;
              outputHash = "sha256-n3Z/jdw4CZvqixnEQX0mS/Fs2ocskLM/s7nt+pd4hPA=";
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

      # Additional packages required to build Nickel on Darwin
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
            "rustfmt"
            "clippy"
          ]
        , channel ? "stable"
        , target ? pkgs.rust.toRustTarget pkgs.stdenv.hostPlatform
        }:
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
              "lsp/client-extension/default.nix"
              "lsp/client-extension/node-env.nix"
              "lsp/client-extension/node-packages.nix"
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
              filterCargoSources
            ];
        };

      # Given a rust toolchain, provide Nickel's Rust dependencies, Nickel, as
      # well as rust tools (like clippy)
      mkCraneArtifacts = { rust ? mkRust { }, noRunBench ? false }:
        let
          craneLib = crane.lib.${system}.overrideToolchain rust;

          # Customize source filtering as Nickel uses non-standard-Rust files like `*.lalrpop`.
          src = filterNickelSrc craneLib.filterCargoSources;

          # set of cargo args common to all builds
          cargoBuildExtraArgs = "--frozen --offline";

          # Build *just* the cargo dependencies, so we can reuse all of that work (e.g. via cachix) when running in CI
          cargoArtifacts = craneLib.buildDepsOnly {
            inherit src;
            cargoExtraArgs = "${cargoBuildExtraArgs} --workspace";
            # pyo3 needs a Python interpreter in the build environment
            # https://pyo3.rs/v0.17.3/building_and_distribution#configuring-the-python-version
            buildInputs = [ pkgs.python3 ];
          };

          buildPackage = pname:
            craneLib.buildPackage {
              inherit
                pname
                src
                cargoArtifacts;

              cargoExtraArgs = "${cargoBuildExtraArgs} --package ${pname}";
            };


        in
        {
          nickel = buildPackage "nickel-lang";
          lsp-nls = buildPackage "nickel-lang-lsp";

          benchmarks = craneLib.mkCargoDerivation {
            inherit src cargoArtifacts;

            pnameSuffix = "-bench";

            buildPhaseCargoCommand = ''
              cargo bench ${pkgs.lib.optionalString noRunBench "--no-run"}
            '';

            doInstallCargoArtifacts = false;
          };

          rustfmt = craneLib.cargoFmt {
            # Notice that unlike other Crane derivations, we do not pass `cargoArtifacts` to `cargoFmt`, because it does not need access to dependencies to format the code.
            inherit src;

            cargoExtraArgs = "--all";

            # `-- --check` is automatically prepended by Crane
            rustFmtExtraArgs = "--color always";
          };

          clippy = craneLib.cargoClippy {
            inherit
              src
              cargoArtifacts;

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
          pkgs.node2nix
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
        { rust ? mkRust { target = "wasm32-unknown-unknown"; }
        , profile ? "release"
        }:
        let
          # Build the various Crane artifacts (dependencies, packages, rustfmt, clippy) for a given Rust toolchain
          craneLib = crane.lib.${system}.overrideToolchain rust;

          # Customize source filtering as Nickel uses non-standard-Rust files like `*.lalrpop`.
          src = filterNickelSrc craneLib.filterCargoSources;

          cargoExtraArgs = "-p nickel-repl --target wasm32-unknown-unknown --frozen --offline";
          # *  --mode no-install prevents wasm-pack from trying to download and
          #   vendor tools like wasm-bindgen, wasm-opt, etc. but use the one
          #   provided by Nix
          # * --no-default-features disable some default features of Nickel that
          #   aren't useful for the WASM REPL (and possibly incompatible with
          #   WASM build)
          wasmPackExtraArgs = "--${profile} --mode no-install -- --no-default-features --frozen --offline";

          # Build *just* the cargo dependencies, so we can reuse all of that work (e.g. via cachix) when running in CI
          cargoArtifacts = craneLib.buildDepsOnly {
            inherit
              src
              cargoExtraArgs;
            doCheck = false;
          };

        in
        craneLib.mkCargoDerivation {
          inherit cargoArtifacts src;

          buildPhaseCargoCommand = ''
            wasm-pack build nickel-wasm-repl ${wasmPackExtraArgs}
          '';

          # nickel-lang.org expects an interface `nickel-repl.wasm`, hence the
          # `ln`
          installPhaseCommand = ''
            mkdir -p $out
            cp -r nickel-wasm-repl/pkg $out/nickel-repl
            ln -s $out/nickel-repl/nickel_repl_bg.wasm $out/nickel-repl/nickel_repl.wasm
          '';

          nativeBuildInputs = [
            rust
            pkgs.wasm-pack
            pkgs.wasm-bindgen-cli
            pkgs.binaryen
          ] ++ missingSysPkgs;
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

      # Build the Nickel VSCode extension. The extension seems to be required
      # for the LSP to work.
      vscodeExtension =
        let node-package = (pkgs.callPackage ./lsp/client-extension { }).package;
        in
        (node-package.override rec {
          pname = "nls-client";
          outputs = [ "vsix" "out" ];
          nativeBuildInputs = with pkgs; [
            # `vsce` depends on `keytar`, which depends on `pkg-config` and `libsecret`
            pkg-config
            libsecret
          ];
          postInstall = ''
            npm run compile
            mkdir -p $vsix
            echo y | npx vsce package -o $vsix/${pname}.vsix
          '';
        }).vsix;

      # Copy the markdown user manual to $out.
      userManual = pkgs.stdenv.mkDerivation {
        name = "nickel-user-manual-${version}";
        src = ./doc/manual;
        installPhase = ''
          mkdir -p $out
          cp -r ./ $out
        '';
      };

      # Generate the stdlib documentation from `nickel doc`.
      stdlibDoc = pkgs.stdenv.mkDerivation {
        name = "nickel-stdlib-doc-${version}";
        src = ./stdlib;
        installPhase = ''
          mkdir -p $out
          for file in *
          do
            module=$(basename $file .ncl)
            ${self.packages."${system}".default}/bin/nickel doc -f "$module.ncl" \
              --output "$out/$module.md"
          done
        '';
      };

    in
    rec {
      packages = {
        inherit (mkCraneArtifacts { })
          nickel
          benchmarks
          lsp-nls;
        default = pkgs.buildEnv {
          name = "nickel";
          paths = [ packages.nickel packages.lsp-nls ];
        };
        nickelWasm = buildNickelWasm { };
        dockerImage = buildDocker packages.nickel; # TODO: docker image should be a passthru
        inherit vscodeExtension;
        inherit userManual;
        inherit stdlibDoc;
      };

      apps = {
        default = {
          type = "app";
          program = "${packages.nickel}/bin/nickel";
        };
      };

      devShells = (forEachRustChannel (channel: {
        name = channel;
        value = makeDevShell { rust = mkRust { inherit channel; rustProfile = "default"; }; };
      })) // {
        default = devShells.stable;
      };

      checks = {
        inherit (mkCraneArtifacts { noRunBench = true; })
          benchmarks
          clippy
          lsp-nls
          nickel
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
