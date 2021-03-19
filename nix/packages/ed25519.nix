{ mkDerivation, base, bytestring, criterion, deepseq, directory
, doctest, filepath, ghc-prim, hlint, lib, QuickCheck
}:
mkDerivation {
  pname = "ed25519";
  version = "0.0.5.0";
  sha256 = "d8a5958ebfa9309790efade64275dc5c441b568645c45ceed1b0c6ff36d6156d";
  revision = "3";
  editedCabalFile = "1yidh86ymzwmp2b449pwim6vvfcs1qgkkncbixw1zmb7wj6v167v";
  libraryHaskellDepends = [ base bytestring ghc-prim ];
  testHaskellDepends = [
    base bytestring directory doctest filepath hlint QuickCheck
  ];
  benchmarkHaskellDepends = [ base bytestring criterion deepseq ];
  homepage = "http://thoughtpolice.github.com/hs-ed25519";
  description = "Ed25519 cryptographic signatures";
  license = lib.licenses.mit;
}
