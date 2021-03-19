{ mkDerivation, base, bytestring, containers, deepseq, lib, mtl
, parsec, QuickCheck, tasty, tasty-quickcheck, text
}:
mkDerivation {
  pname = "HsYAML";
  version = "0.2.1.0";
  sha256 = "60f727d5c90e693ef71df7dcbed8f40b66d2db11375528043e0326749e861f83";
  revision = "1";
  editedCabalFile = "0bfwdwwj5wgqrrbw1cwaxwxy9970dzln7w20f21mlg2l374wnqvf";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers deepseq mtl parsec text
  ];
  testHaskellDepends = [
    base bytestring containers mtl QuickCheck tasty tasty-quickcheck
    text
  ];
  homepage = "https://github.com/hvr/HsYAML";
  description = "Pure Haskell YAML 1.2 processor";
  license = lib.licenses.gpl2;
}
