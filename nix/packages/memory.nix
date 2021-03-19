{ mkDerivation, base, basement, bytestring, deepseq, foundation
, ghc-prim, lib
}:
mkDerivation {
  pname = "memory";
  version = "0.15.0";
  sha256 = "e3ff892c1a94708954d0bb2c4f4ab81bc0f505352d95095319c462db1aeb3529";
  revision = "1";
  editedCabalFile = "136qfj1cbg9571mlwywaqml75ijx3pcgvbpbgwxrqsl71ssj8w5y";
  libraryHaskellDepends = [
    base basement bytestring deepseq ghc-prim
  ];
  testHaskellDepends = [ base basement bytestring foundation ];
  homepage = "https://github.com/vincenthz/hs-memory";
  description = "memory and related abstraction stuff";
  license = lib.licenses.bsd3;
}
