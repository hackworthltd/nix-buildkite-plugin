{
  description = "A Buildkite plugin for Nix builds.";

  inputs = {
    haskell-nix.url = "github:input-output-hk/haskell.nix";

    # We use this for some convenience functions only.
    hacknix.url = "github:hackworthltd/hacknix";

    pre-commit-hooks-nix.url = "github:cachix/pre-commit-hooks.nix";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    flake-parts.url = "github:hercules-ci/flake-parts";

    # Let haskell.nix dictate the nixpkgs we use, as that will ensure
    # better haskell.nix cache hits.
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    hacknix.inputs.nixpkgs.follows = "nixpkgs";
    pre-commit-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";

    haskell-language-server.url = "github:haskell/haskell-language-server/748603e1cf4d85b3aa31bff4d91edd4b8b3fa66b";
  };

  outputs = inputs@ { flake-parts, ... }:
    let
      allOverlays = [
        inputs.haskell-nix.overlay
        inputs.self.overlays.default
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
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      debug = true;

      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
      ];

      systems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { config, pkgs, system, ... }:
        let
          # haskell.nix does a lot of heavy lifiting for us and gives us a
          # flake for our Cabal project with the following attributes:
          # `checks`, `apps`, and `packages`.
          haskellNixFlake = pkgs.nix-buildkite-plugin.flake { };

          # Filter out any file in this repo that doesn't affect a Cabal
          # build or Haskell-related check. (Note: this doesn't need to be
          # 100% accurate, it's just an optimization to cut down on
          # extraneous Nix builds.)
          onlyHaskellSrc =
            let
              inherit (pkgs.haskell-nix) haskellSourceFilter;
              inherit (pkgs.haskell-nix.haskellLib) cleanGit cleanSourceWith;

              sourceFilter = name: type:
                let baseName = baseNameOf (toString name);
                in ! (
                  baseName == ".buildkite" ||
                  baseName == ".github" ||
                  baseName == "CHANGELOG.md" ||
                  baseName == "LICENSE" ||
                  baseName == "README.md" ||
                  pkgs.lib.hasPrefix "cabal.project.local" baseName ||
                  baseName == "default.nix" ||
                  baseName == "flake.lock" ||
                  baseName == "flake.nix" ||
                  baseName == "hooks" ||
                  baseName == "jobs.nix" ||
                  baseName == "nix" ||
                  baseName == "plugin.yml" ||
                  baseName == "shell.nix"
                );
            in
            cleanSourceWith {
              filter = haskellSourceFilter;
              name = "nix-buildkite-plugin-src";
              src = cleanSourceWith
                {
                  filter = sourceFilter;
                  src = cleanGit
                    {
                      src = ./.;
                    };
                };
            };
        in
        {
          # We need a `pkgs` that includes our own overlays within
          # `perSystem`. This isn't done by default, so we do this
          # workaround. See:
          #
          # https://github.com/hercules-ci/flake-parts/issues/106#issuecomment-1399041045
          _module.args.pkgs = import inputs.nixpkgs
            {
              inherit system;
              config = {
                allowUnfree = true;
                allowBroken = true;
              };
              overlays = allOverlays;
            };

          pre-commit =
            let
              # Override the default nix-pre-commit-hooks tools with the version
              # we're using.
              haskellNixTools = pkgs.haskell-nix.tools ghcVersion {
                hlint = "latest";
                fourmolu = fourmoluVersion;
              };
            in
            {
              check.enable = true;
              settings = {
                src = ./.;
                hooks = {
                  hlint.enable = true;
                  fourmolu.enable = true;
                  nixpkgs-fmt.enable = true;
                  shellcheck.enable = true;
                  actionlint.enable = true;
                };

                # We need to force these due to
                #
                # https://github.com/cachix/pre-commit-hooks.nix/issues/204
                tools = {
                  nixpkgs-fmt = pkgs.lib.mkForce pkgs.nixpkgs-fmt;
                  hlint = pkgs.lib.mkForce haskellNixTools.hlint;
                  fourmolu = pkgs.lib.mkForce haskellNixTools.fourmolu;
                };

                excludes = [
                  ".buildkite/"
                ];
              };
            };

          packages = {
            inherit (pkgs) nix-buildkite;
          } // (pkgs.lib.optionalAttrs (system == "x86_64-linux") {
            inherit (pkgs) nix-buildkite-docker-image;
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
              inherit (pkgs) nix-buildkite;
            })
            // haskellNixFlake.apps;

          devShells.default = haskellNixFlake.devShells.default;
        };

      flake =
        let
          # See above, we need to use our own `pkgs` within the flake.
          pkgs = import inputs.nixpkgs
            {
              system = "x86_64-linux";
              config = {
                allowUnfree = true;
                allowBroken = true;
              };
              overlays = allOverlays;
            };
        in
        {
          overlays.default = (final: prev:
            let
              ghc982Tools = final.haskell-nix.tools "ghc982" {
                hlint = "latest";
                cabal-fmt = "latest";
                ghcid = "latest";
              };

              nix-buildkite-plugin = final.haskell-nix.cabalProject {
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
                  };

                  buildInputs = (with final; [
                    # For Language Server support.
                    nodejs_22

                    nixpkgs-fmt
                    cabal-fmt
                  ]);

                  shellHook = ''
                    export HIE_HOOGLE_DATABASE="$(cat $(${final.which}/bin/which hoogle) | sed -n -e 's|.*--database \(.*\.hoo\).*|\1|p')"
                  '';
                };
              };

              nixBuildkiteFlake = nix-buildkite-plugin.flake { };

              nix-buildkite-docker-image = final.dockerTools.buildLayeredImage {
                name = "nix-buildkite";
                tag = version;
                contents = (with final;[
                  nix
                  nix-buildkite
                ])
                ++ (with final; [
                  # These are helpful for debugging broken images.
                  bashInteractive
                  coreutils
                  lsof
                  procps
                ]);

                config = {
                  # Note that we can't set
                  # "org.opencontainers.image.created" here because
                  # it would introduce an impurity. If we want to
                  # set it, we'll need to set it when we push to a
                  # registry.
                  Labels = {
                    "org.opencontainers.image.source" =
                      "https://github.com/hackworthltd/nix-buildkite-plugin";
                    "org.opencontainers.image.documentation" =
                      "https://github.com/hackworthltd/nix-buildkite-plugin";
                    "org.opencontainers.image.title" = "nix-buildkite";
                    "org.opencontainers.image.description" =
                      "Create Buildkite pipelines from Nix attributes.";
                    "org.opencontainers.image.version" = version;
                    "org.opencontainers.image.authors" =
                      "src@hackworthltd.com";
                    "org.opencontainers.image.licenses" = "BSD-3-Clause";
                    "org.opencontainers.image.vendor" = "Hackworth Ltd";
                    "org.opencontainers.image.url" =
                      "https://github.com/hackworthltd/nix-buildkite-plugin";
                    "org.opencontainers.image.revision" = inputs.self.rev or "dirty";
                  };
                };
              };

            in
            {
              inherit nix-buildkite-plugin;

              nix-buildkite = nixBuildkiteFlake.packages."nix-buildkite-plugin:exe:nix-buildkite";
              inherit nix-buildkite-docker-image;

              inherit (ghc982Tools) cabal-fmt hlint ghcid;
            }
          );

          hydraJobs = {
            inherit (inputs.self) checks;
            inherit (inputs.self) packages;
            inherit (inputs.self) devShells;

            required = pkgs.releaseTools.aggregate {
              name = "required-nix-ci";
              constituents = builtins.map builtins.attrValues (with inputs.self.hydraJobs; [
                packages.x86_64-linux
                packages.aarch64-darwin
                checks.x86_64-linux
                checks.aarch64-darwin
              ]);
              meta.description = "Required Nix CI builds";
            };
          };

          ciJobs = inputs.hacknix.lib.flakes.recurseIntoHydraJobs inputs.self.hydraJobs;
        };
    };
}
