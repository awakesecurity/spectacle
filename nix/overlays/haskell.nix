{ ghcVersion }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghcVersion}" = prev.haskell.packages."${ghcVersion}".extend 
        (final-haskell-packages: prev-haskell-packages: {
          logict = prev-haskell-packages.logict_0_8_0_0;
          spectacle = final-haskell-packages.callCabal2nix "spectacle" ../../. { };
        });
    };
  };
}