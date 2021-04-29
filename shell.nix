let
  pkgs = import ./nix/pkgs.nix;

  spectacle = pkgs.haskellPackages.spectacle;

in
spectacle.env.overrideAttrs (old: {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
