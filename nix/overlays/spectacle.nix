{ ghcVersion }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghcVersion}" = prev.haskell.packages."${ghcVersion}".extend (self: _: {
        spectacle = self.callCabal2nix "spectacle" ../../. { };
      });
    };
  };
}