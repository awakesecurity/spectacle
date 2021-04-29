let
  pkgs = import ./nix/pkgs.nix;

in
pkgs.haskell.lib.buildStrictly pkgs.haskellPackages.spectacle
