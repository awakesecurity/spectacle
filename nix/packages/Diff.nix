{ mkDerivation, array, base, directory, lib, pretty, process
, QuickCheck, test-framework, test-framework-quickcheck2
}:
mkDerivation {
  pname = "Diff";
  version = "0.4.0";
  sha256 = "7290ac098ad8b4748b9c10e494cc85ba54af688226ae69a465aa7b4c73f149c7";
  libraryHaskellDepends = [ array base pretty ];
  testHaskellDepends = [
    array base directory pretty process QuickCheck test-framework
    test-framework-quickcheck2
  ];
  description = "O(ND) diff algorithm in haskell";
  license = lib.licenses.bsd3;
}
