{ mkDerivation, base, base-compat, base-orphans, deepseq, HUnit
, lib, QuickCheck, tagged, tasty, tasty-hunit, tasty-quickcheck
, time
}:
mkDerivation {
  pname = "time-compat";
  version = "1.9.5";
  sha256 = "3126b267d19f31d52a3c36f13a8788be03242f829a5bddd8a3084e134d01e3a6";
  libraryHaskellDepends = [ base base-orphans deepseq time ];
  testHaskellDepends = [
    base base-compat deepseq HUnit QuickCheck tagged tasty tasty-hunit
    tasty-quickcheck time
  ];
  homepage = "https://github.com/haskellari/time-compat";
  description = "Compatibility package for time";
  license = lib.licenses.bsd3;
}
