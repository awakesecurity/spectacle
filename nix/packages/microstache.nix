{ mkDerivation, aeson, base, bytestring, containers, deepseq
, directory, filepath, hspec, lib, parsec, text, transformers
, unordered-containers, vector
}:
mkDerivation {
  pname = "microstache";
  version = "1.0.1.2";
  sha256 = "336e2505889b9af2ea8939a606ec35bc67bab1c9f0eb26bcdbc7b3f24350acf5";
  libraryHaskellDepends = [
    aeson base containers deepseq directory filepath parsec text
    transformers unordered-containers vector
  ];
  testHaskellDepends = [
    aeson base bytestring containers hspec parsec text
  ];
  homepage = "https://github.com/phadej/microstache";
  description = "Mustache templates for Haskell";
  license = lib.licenses.bsd3;
}
