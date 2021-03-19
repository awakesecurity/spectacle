{ mkDerivation, base, base-orphans, deepseq, ghc-prim, lib
, QuickCheck, quickcheck-classes-base, semigroups, tagged, tasty
, tasty-quickcheck, transformers, transformers-compat
}:
mkDerivation {
  pname = "primitive";
  version = "0.7.1.0";
  sha256 = "6bebecfdf2a57787d9fd5231bfd612b65a92edd7b33a973b2a0f11312b89a3f0";
  revision = "2";
  editedCabalFile = "1m08slj8m596z4pqsw3ag25ijhzlv9ki809ydh4nbin141bpsdgn";
  libraryHaskellDepends = [ base deepseq transformers ];
  testHaskellDepends = [
    base base-orphans ghc-prim QuickCheck quickcheck-classes-base
    semigroups tagged tasty tasty-quickcheck transformers
    transformers-compat
  ];
  homepage = "https://github.com/haskell/primitive";
  description = "Primitive memory-related operations";
  license = lib.licenses.bsd3;
}
