let
  pkgs = import ./nix/pkgs.nix;

in
{
  spectacle = pkgs.haskell.lib.buildStrictly pkgs.haskellPackages.spectacle;
}
