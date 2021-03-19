{ mkDerivation, base, directory, filepath, hspec-meta, lib
, QuickCheck
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.7.8";
  sha256 = "c7587dddf1e0ad2d534441aaca00646262b82adcaa625f9697647f4c7cd55e7c";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base directory filepath hspec-meta QuickCheck
  ];
  testToolDepends = [ hspec-meta ];
  homepage = "http://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = lib.licenses.mit;
}
