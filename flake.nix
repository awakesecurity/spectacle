{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    let
      systems = with flake-utils.lib; [
        system.x86_64-linux
        system.aarch64-linux
        system.x86_64-darwin
        system.aarch64-darwin
      ];

      ghcVersion = "ghc92";

    in flake-utils.lib.eachSystem systems (system:
      let
        pkgs = import nixpkgs { 
          inherit system; 

          config.packageOverrides = import (nix/overlays.nix) {
            inherit ghcVersion;
          };
        };

        haskell-packages = pkgs.haskell.packages."${ghcVersion}";

        packages = flake-utils.lib.flattenTree rec {
          inherit (haskell-packages)
            haskell-language-server
            fourmolu;

          default = haskell-packages.spectacle;
        };

        apps = {
          fourmolu = { 
            type = "app"; 
            program = "${packages.fourmolu}/bin/fourmolu"; 
          };
          haskell-language-server = { 
            type = "app"; 
            program = "${packages.haskell-language-server}/bin/haskell-language-server"; 
          };
        };

        devShells = {
          default = haskell-packages.shellFor {
            name = "spectacle";

            buildInputs = with haskell-packages; [
              fourmolu
              haskell-language-server
            ];

            packages = pkgs: [
              pkgs.spectacle
            ];
          };
        };

      in { 
        inherit apps devShells packages; 
      });
}
