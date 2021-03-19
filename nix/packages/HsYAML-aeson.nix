{ mkDerivation, aeson, base, bytestring, containers, HsYAML, lib
, mtl, scientific, text, unordered-containers, vector
}:
mkDerivation {
  pname = "HsYAML-aeson";
  version = "0.2.0.0";
  sha256 = "cfb9634b43fcaddb5a520838119ba4b02b18423a35471fef5a805d6004e75d8b";
  revision = "2";
  editedCabalFile = "0sf4clxx3i3s6666w3il65fijx2xmgb1mxml9jgc5y40sj3qb3mm";
  libraryHaskellDepends = [
    aeson base bytestring containers HsYAML mtl scientific text
    unordered-containers vector
  ];
  description = "JSON to YAML Adapter";
  license = lib.licenses.gpl2Plus;
}
