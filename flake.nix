{
  description = "A Buildkite plugin for Nix builds.";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # We use this for some convenience functions only.
    hacknix.url = "github:hackworthltd/hacknix";

    pre-commit-hooks-nix.url = "github:cachix/git-hooks.nix";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    flake-parts.url = "github:hercules-ci/flake-parts";

    treefmt-nix.url = "github:numtide/treefmt-nix";

    # Let haskell.nix dictate the nixpkgs we use, as that will ensure
    # better haskell.nix cache hits.
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    hacknix.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
    treefmt-nix.inputs.nixpkgs.follows = "nixpkgs";

    haskell-language-server.url = "github:haskell/haskell-language-server/748603e1cf4d85b3aa31bff4d91edd4b8b3fa66b";
    cabal-fmt.url = "github:phadej/cabal-fmt";
    cabal-fmt.flake = false;

    nix-eval-jobs.url = "github:nix-community/nix-eval-jobs";
  };

  outputs =
    inputs@{ flake-parts, ... }:
    let
      allOverlays = [
        inputs.haskell-nix.overlay
      ];

      version =
        let
          v = inputs.self.rev or inputs.self.lastModifiedDate;
        in
        builtins.trace "nix-buildkite version is ${v}" "git-${v}";

      ghcVersion = "ghc9122";

      # Fourmolu updates often alter formatting arbitrarily, and we want to
      # have more control over this.
      fourmoluVersion = "0.18.0.0";

      # cabal-fmt needs an override for GHC > 9.8.1.
      cabal-fmt-override = {
        src = inputs.cabal-fmt;
        cabalProjectLocal = ''
          allow-newer: cabal-fmt:base
        '';
      };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;

      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem =
        {
          config,
          pkgs,
          system,
          ...
        }:
        let
          nixBuildkitePlugin = pkgs.haskell-nix.cabalProject {
            compiler-nix-name = ghcVersion;
            src = ./.;
            evalSystem = "x86_64-linux";

            modules = [
              {
                packages = {
                  nix-buildkite-plugin = {
                    ghcOptions = [ "-Werror" ];
                  };
                };
              }
              {
                # Build everything with -O2.
                configureFlags = [ "-O2" ];

                # Generate HIE files for everything.
                writeHieFiles = true;

                # Generate nice Haddocks & a Hoogle index for
                # everything.
                doHaddock = true;
                doHyperlinkSource = true;
                doQuickjump = true;
                doHoogle = true;
              }
              {
                # These packages don't/can't generate HIE files. See:
                # https://github.com/input-output-hk/haskell.nix/issues/1242
                packages.mtl-compat.writeHieFiles = false;
              }
            ];

            shell = {
              exactDeps = true;
              withHoogle = true;
              tools = {
                ghcid = "latest";

                haskell-language-server = {
                  src = inputs.haskell-language-server;
                  cabalProjectLocal = ''
                    allow-newer: haddock-library:base
                  '';
                };

                implicit-hie = "latest";

                cabal = "latest";
                hlint = "latest";

                fourmolu = fourmoluVersion;
                cabal-fmt = cabal-fmt-override;
              };

              inputsFrom = [
                config.treefmt.build.devShell
              ];

              buildInputs = (
                with pkgs;
                [
                  actionlint
                  nixd
                  vscode-langservers-extracted
                  nixfmt-rfc-style

                  # For Language Server support.
                  nodejs_22
                ]
              );

              shellHook = ''
                ${config.pre-commit.installationScript}
                export HIE_HOOGLE_DATABASE="$(cat $(${pkgs.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
              '';
            };
          };

          # haskell.nix does a lot of heavy lifiting for us and gives us a
          # flake for our Cabal project with the following attributes:
          # `checks`, `apps`, and `packages`.
          haskellNixFlake = nixBuildkitePlugin.flake { };

          nix-buildkite = haskellNixFlake.packages."nix-buildkite-plugin:exe:nix-buildkite";

          nix-buildkite-docker-image = pkgs.dockerTools.buildLayeredImage {
            name = "nix-buildkite";
            tag = version;
            contents =
              (with pkgs; [
                nix
              ])
              ++ (with pkgs; [
                # These are helpful for debugging broken images.
                bashInteractive
                coreutils
                lsof
                procps
              ])
              ++ [
                nix-buildkite
              ];

            config = {
              # Note that we can't set
              # "org.opencontainers.image.created" here because
              # it would introduce an impurity. If we want to
              # set it, we'll need to set it when we push to a
              # registry.
              Labels = {
                "org.opencontainers.image.source" = "https://github.com/hackworthltd/nix-buildkite-plugin";
                "org.opencontainers.image.documentation" = "https://github.com/hackworthltd/nix-buildkite-plugin";
                "org.opencontainers.image.title" = "nix-buildkite";
                "org.opencontainers.image.description" = "Create Buildkite pipelines from Nix attributes.";
                "org.opencontainers.image.version" = version;
                "org.opencontainers.image.authors" = "src@hackworthltd.com";
                "org.opencontainers.image.licenses" = "BSD-3-Clause";
                "org.opencontainers.image.vendor" = "Hackworth Ltd";
                "org.opencontainers.image.url" = "https://github.com/hackworthltd/nix-buildkite-plugin";
                "org.opencontainers.image.revision" = inputs.self.rev or "dirty";
              };
            };
          };

          # Filter out any file in this repo that doesn't affect a Cabal
          # build or Haskell-related check. (Note: this doesn't need to be
          # 100% accurate, it's just an optimization to cut down on
          # extraneous Nix builds.)
          onlyHaskellSrc =
            let
              inherit (pkgs.haskell-nix) haskellSourceFilter;
              inherit (pkgs.haskell-nix.haskellLib) cleanGit cleanSourceWith;

              sourceFilter =
                name: type:
                let
                  baseName = baseNameOf (toString name);
                in
                !(
                  baseName == ".buildkite"
                  || baseName == ".github"
                  || baseName == "CHANGELOG.md"
                  || baseName == "LICENSE"
                  || baseName == "README.md"
                  || pkgs.lib.hasPrefix "cabal.project.local" baseName
                  || baseName == "default.nix"
                  || baseName == "flake.lock"
                  || baseName == "flake.nix"
                  || baseName == "hooks"
                  || baseName == "jobs.nix"
                  || baseName == "nix"
                  || baseName == "plugin.yml"
                  || baseName == "shell.nix"
                );
            in
            cleanSourceWith {
              filter = haskellSourceFilter;
              name = "nix-buildkite-plugin-src";
              src = cleanSourceWith {
                filter = sourceFilter;
                src = cleanGit {
                  src = ./.;
                };
              };
            };

          with-nix-eval-jobs = pkgs.writeShellApplication {
            name = "with-nix-eval-jobs";
            runtimeInputs =
              (with pkgs; [
                jq
              ])
              ++ [
                inputs.nix-eval-jobs.packages.${system}.nix-eval-jobs
                nix-buildkite
              ];
            text = builtins.readFile ./scripts/with-nix-eval-jobs.sh;
          };
        in
        {
          # We need a `pkgs` that includes our own overlays within
          # `perSystem`. This isn't done by default, so we do this
          # workaround. See:
          #
          # https://github.com/hercules-ci/flake-parts/issues/106#issuecomment-1399041045
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
            overlays = allOverlays;
          };

          formatter = pkgs.nixfmt-rfc-style;

          pre-commit = {
            check.enable = true;
            settings = {
              src = ./.;
              hooks = {
                treefmt.enable = true;
              };
            };
          };

          packages =
            {
              inherit nix-buildkite;
              inherit with-nix-eval-jobs;
            }
            // (pkgs.lib.optionalAttrs (system == "x86_64-linux") {
              inherit nix-buildkite-docker-image;
            })
            // haskellNixFlake.packages;

          checks = haskellNixFlake.checks;

          apps =
            let
              mkApp = pkg: script: {
                type = "app";
                program = "${pkg}/bin/${script}";
              };
            in
            (pkgs.lib.mapAttrs (name: pkg: mkApp pkg name) {
              inherit nix-buildkite;
            })
            // haskellNixFlake.apps;

          treefmt.config =
            let
              haskellNixTools = pkgs.haskell-nix.tools ghcVersion {
                fourmolu = fourmoluVersion;
                cabal-fmt = cabal-fmt-override;
                hlint = "latest";
              };
            in
            {
              projectRootFile = "flake.nix";

              programs.nixfmt.enable = true;

              programs.hlint = {
                enable = true;
                package = haskellNixTools.hlint;
              };
              programs.cabal-fmt = {
                enable = true;
                package = haskellNixTools.cabal-fmt;
              };
              programs.fourmolu = {
                enable = true;
                package = haskellNixTools.fourmolu;
              };
              programs.shellcheck.enable = true;
              programs.actionlint.enable = true;

              settings.on-unmatched = "info";
            };

          devShells.default = haskellNixFlake.devShells.default;
        };

      flake =
        let
          # See above, we need to use our own `pkgs` within the flake.
          pkgs = import inputs.nixpkgs {
            system = "x86_64-linux";
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
            overlays = allOverlays;
          };
        in
        {
          hydraJobs = {
            inherit (inputs.self) checks;
            inherit (inputs.self) packages;
            inherit (inputs.self) devShells;

            required = pkgs.releaseTools.aggregate {
              name = "required-nix-ci";
              constituents = builtins.map builtins.attrValues (
                with inputs.self.hydraJobs;
                [
                  packages.x86_64-linux
                  packages.aarch64-darwin
                  checks.x86_64-linux
                  checks.aarch64-darwin
                  devShells.x86_64-linux
                  devShells.aarch64-darwin
                ]
              );
              meta.description = "Required Nix CI builds";
            };
          };

          ciJobs = inputs.hacknix.lib.flakes.recurseIntoHydraJobs inputs.self.hydraJobs;
        };
    };
}
