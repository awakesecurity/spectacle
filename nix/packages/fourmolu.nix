{ mkDerivation, aeson, base, bytestring, containers, directory
, dlist, exceptions, filepath, ghc-lib-parser, gitrev, hspec
, hspec-discover, HsYAML, HsYAML-aeson, lib, mtl
, optparse-applicative, path, path-io, syb, text
}:
mkDerivation {
  pname = "fourmolu";
  version = "0.3.0.0";
  sha256 = "ba7201c78ee61665eaf1fce10cd297dd5383d36053cd3984e41a5094d96e096d";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson base bytestring containers directory dlist exceptions
    filepath ghc-lib-parser HsYAML HsYAML-aeson mtl syb text
  ];
  executableHaskellDepends = [
    base directory ghc-lib-parser gitrev optparse-applicative text
  ];
  testHaskellDepends = [
    base containers filepath hspec path path-io text
  ];
  testToolDepends = [ hspec-discover ];
  homepage = "https://github.com/parsonsmatt/fourmolu";
  description = "A formatter for Haskell source code";
  license = lib.licenses.bsd3;
}
