{ mkDerivation, base, deepseq, lib, template-haskell, transformers
}:
mkDerivation {
  pname = "tagged";
  version = "0.8.6.1";
  sha256 = "f5e0fcf95f0bb4aa63f428f2c01955a41ea1a42cfcf39145ed631f59a9616c02";
  libraryHaskellDepends = [
    base deepseq template-haskell transformers
  ];
  homepage = "http://github.com/ekmett/tagged";
  description = "Haskell 98 phantom types to avoid unsafely passing dummy arguments";
  license = lib.licenses.bsd3;
}
