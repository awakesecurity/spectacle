{ mkDerivation, base, extensible-exceptions, lib, QuickCheck
, random, test-framework
}:
mkDerivation {
  pname = "test-framework-quickcheck2";
  version = "0.3.0.5";
  sha256 = "c9f678d4ec30599172eb887031f0bce2012b532daeb713836bd912bff64eee59";
  revision = "2";
  editedCabalFile = "1apgf91van2070m6jhj9w3h2xmr42r4kk0da9crq9994hd8zwny2";
  libraryHaskellDepends = [
    base extensible-exceptions QuickCheck random test-framework
  ];
  homepage = "http://haskell.github.io/test-framework/";
  description = "QuickCheck-2 support for the test-framework package";
  license = lib.licenses.bsd3;
}
