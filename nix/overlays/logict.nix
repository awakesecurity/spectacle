{ ghcVersion }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghcVersion}" = prev.haskell.packages."${ghcVersion}".extend (_: _: {
        logict = prev.haskell.packages."${ghcVersion}".logict_0_8_0_0;
      });
    };
  };
}