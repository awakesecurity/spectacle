let
  pkgs = import ./nix/pkgs.nix;

  spectacle-shell = (pkgs.haskellPackages.shellFor {
    packages = p: [
      p.spectacle
    ];
    buildInputs = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghcid ];
  });

in {
  inherit pkgs;
  inherit (pkgs.haskellPackages) fourmolu dhall-yaml dhall-nix;
  inherit spectacle-shell;

  spectacle =
    pkgs.haskell.lib.appendConfigureFlags pkgs.haskellPackages.spectacle [
      "--ghc-options=-Werror"
    ];
}
