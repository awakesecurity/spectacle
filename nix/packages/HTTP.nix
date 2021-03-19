{ mkDerivation, array, base, bytestring, deepseq, httpd-shed, HUnit
, lib, mtl, network, network-uri, parsec, pureMD5, split
, test-framework, test-framework-hunit, time
}:
mkDerivation {
  pname = "HTTP";
  version = "4000.3.15";
  sha256 = "0d6b368e43001c046660e0e209bf9795dc990cb45016447fcf92e822c22e1594";
  revision = "2";
  editedCabalFile = "1rkazrbxfpx7bhp6fhgx97j58i4ccyp8bxjgdhr33cpv1b2cza9b";
  libraryHaskellDepends = [
    array base bytestring mtl network network-uri parsec time
  ];
  testHaskellDepends = [
    base bytestring deepseq httpd-shed HUnit mtl network network-uri
    pureMD5 split test-framework test-framework-hunit
  ];
  homepage = "https://github.com/haskell/HTTP";
  description = "A library for client-side HTTP";
  license = lib.licenses.bsd3;
}
