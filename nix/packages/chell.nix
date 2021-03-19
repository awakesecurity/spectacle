{ mkDerivation, ansi-terminal, base, bytestring, lib, options
, patience, random, template-haskell, text, transformers
}:
mkDerivation {
  pname = "chell";
  version = "0.5";
  sha256 = "cf99f7d015fc91ad9d6bd159a420b577baeb7ac52b55250ac21ecce5742c04c5";
  revision = "1";
  editedCabalFile = "1q93wrw03ix4cmnkz3lzkixcvvizw6i2ia2zifdfak1dvxnblxk0";
  libraryHaskellDepends = [
    ansi-terminal base bytestring options patience random
    template-haskell text transformers
  ];
  homepage = "https://github.com/typeclasses/chell";
  description = "A simple and intuitive library for automated testing";
  license = lib.licenses.mit;
}
