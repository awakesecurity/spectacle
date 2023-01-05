{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        spectacle = self.callCabal2nix "spectacle" ../../. { };
      });
    };
  };
}