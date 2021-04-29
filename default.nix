let
  pkgs = import ./nix/pkgs.nix;

in {
  inherit pkgs;
  inherit (pkgs.haskellPackages) fourmolu dhall-yaml dhall-nix;

  spectacle =
    pkgs.haskell.lib.appendConfigureFlags pkgs.haskellPackages.spectacle [
      "--ghc-options=-Werror"
    ];
}
