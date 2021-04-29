let
  pkgs = import ./nix/pkgs.nix;

  spectacle = pkgs.haskellPackages.spectacle;

in
spectacle.env.overrideAttrs (old: {
  buildInputs = (old.buildInputs or []) ++ [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
