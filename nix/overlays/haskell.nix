{ ghcVersion }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghcVersion}" = prev.haskell.packages."${ghcVersion}".extend 
        (final-haskell-packages: prev-haskell-packages: {
          spectacle = final-haskell-packages.callCabal2nix "spectacle" ../../. { };
        });
    };
  };
}