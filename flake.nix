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

      ghcVersion = "9.2.4";

    in flake-utils.lib.eachSystem systems (system:
      let
        overlays = [ ];
        pkgs = import nixpkgs { inherit system overlays; };

        haskellPackages =
          # Mangle the version string into the proper attribute name, then use that.
          let attr = with pkgs.lib; "ghc" + (concatStrings (splitString "." ghcVersion));
          in pkgs.haskell.packages."${attr}";

        packages = flake-utils.lib.flattenTree rec {
          default = spectacle;
          spectacle = haskellPackages.callCabal2nix "spectacle" ./. {
            logict = haskellPackages.logict_0_8_0_0;
          };
          inherit (haskellPackages) fourmolu;
        };

        apps = {
          fourmolu = { type = "app"; program = "${packages.fourmolu}/bin/fourmolu"; };
        };

      in { inherit packages apps; });
}
