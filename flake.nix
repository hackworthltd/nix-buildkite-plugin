{
  description = "A Buildkite plugin for Nix builds.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/22.05";

    # We use this for some convenience functions only.
    hacknix.url = "github:hackworthltd/hacknix";

    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;

    flake-parts.url = "github:hercules-ci/flake-parts";

    hacknix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@ { flake-parts, ... }:
    let
    in
    flake-parts.lib.mkFlake { inherit inputs; }
      {
        debug = true;

        imports = [
        ];
        systems = [ "x86_64-linux" "aarch64-darwin" ];

        perSystem = { config, pkgs, system, ... }: {
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
              overlays = [ inputs.self.overlays.default ];
            };

          packages = {
            inherit (pkgs.haskellPackages) nix-buildkite;
          };

          checks = { };

          apps =
            let
              mkApp = pkg: script: {
                type = "app";
                program = "${pkg}/bin/${script}";
              };
            in
            (pkgs.lib.mapAttrs (name: pkg: mkApp pkg name) {
              inherit (pkgs.haskellPackages) nix-buildkite;
            });

          devShells.default = pkgs.haskellPackages.nix-buildkite.env;
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
                overlays = [ inputs.self.overlays.default ];
              };
          in
          {
            overlays.default = (final: prev:
              let
                inherit (prev) haskell haskellPackages;

                inherit (haskellPackages) callCabal2nix;

                inherit (haskell.lib) appendConfigureFlag packagesFromDirectory;

                inherit (prev.lib) composeExtensions;

                WError =
                  drv: appendConfigureFlag drv "--ghc-option=-Werror";

                configurations =
                  self: super: {
                    nix-buildkite = WError (callCabal2nix "nix-buildkite" (builtins.path { path = ./.; name = "nix-buildkite"; }) { });
                  };

              in
              {
                haskellPackages =
                  prev.haskellPackages.override
                    (
                      old:
                      {
                        overrides = configurations;
                      }
                    );
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

