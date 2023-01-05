{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (_: _: {
        logict = prev.haskell.packages."${ghc}".logict_0_8_0_0;
      });
    };
  };
}