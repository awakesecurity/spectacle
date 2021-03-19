{ mkDerivation, base, base16-bytestring, bytestring, criterion, lib
, SHA, tasty, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "cryptohash-sha512";
  version = "0.11.100.1";
  sha256 = "10698bb9575eaa414a65d9644caa9408f9276c63447406e0a4faef91db1071a9";
  revision = "5";
  editedCabalFile = "0ccvr3sp7mnllbd430l91b5ij70gg3g05nm9n6qaxzx2vaqdlyc2";
  libraryHaskellDepends = [ base bytestring ];
  testHaskellDepends = [
    base base16-bytestring bytestring SHA tasty tasty-hunit
    tasty-quickcheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion ];
  homepage = "https://github.com/hvr/cryptohash-sha512";
  description = "Fast, pure and practical SHA-512 implementation";
  license = lib.licenses.bsd3;
}
