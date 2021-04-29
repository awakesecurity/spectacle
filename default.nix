let
  pkgs = import ./nix/pkgs.nix;

in {
  spectacle =
    pkgs.haskell.lib.appendConfigureFlags pkgs.haskellPackages.spectacle [
      "--ghc-options=-Werror"
    ];
}
