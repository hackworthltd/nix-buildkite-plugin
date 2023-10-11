{
  description = "A test flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.05";

    flake-parts.url = "github:hercules-ci/flake-parts/c9afaba3dfa4085dbd2ccb38dfade5141e33d9d4";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";

    # We use this for some convenience functions only.
    hacknix.url = "github:hackworthltd/hacknix/d4ac93da22f1d83e2a264b63c6f5d615e21e46b7";
    hacknix.inputs.nixpkgs.follows = "nixpkgs";

    flake-compat.url = "github:edolstra/flake-compat/v1.0.1";
    flake-compat.flake = false;
  };

  outputs = inputs@ { flake-parts, ... }: flake-parts.lib.mkFlake { inherit inputs; } {
    systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

    perSystem = { config, pkgs, system, ... }:
      let
        hello = pkgs.callPackage ./nix/pkgs/hello { };
        syntheticDeps = pkgs.symlinkJoin {
          name = "synthetic-deps";
          paths = [ hello ];
        };
      in
      {
        packages = {
          inherit hello syntheticDeps;
        }
        // (pkgs.lib.optionalAttrs (system == "x86_64-linux" || system == "aarch64-linux")) {
          test-docker-image = pkgs.dockerTools.buildLayeredImage {
            name = "test-docker-image";
            tag = "v1.0";
            contents = ([
              syntheticDeps
            ]);
          };
        };
      };

    flake = {
      hydraJobs = {
        inherit (inputs.self) checks;
        inherit (inputs.self) packages;
      };
      ciJobs = inputs.hacknix.lib.flakes.recurseIntoHydraJobs inputs.self.hydraJobs;
    };
  };
}

