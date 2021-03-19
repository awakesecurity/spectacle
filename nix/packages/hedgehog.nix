{ mkDerivation, ansi-terminal, async, base, bytestring
, concurrent-output, containers, deepseq, directory, erf
, exceptions, lib, lifted-async, mmorph, monad-control, mtl
, pretty-show, primitive, random, resourcet, stm, template-haskell
, text, time, transformers, transformers-base, wl-pprint-annotated
}:
mkDerivation {
  pname = "hedgehog";
  version = "1.0.4";
  sha256 = "bf7157d433a453587343186d739e178abf1b7a5ec916555630889cd7c104469a";
  libraryHaskellDepends = [
    ansi-terminal async base bytestring concurrent-output containers
    deepseq directory erf exceptions lifted-async mmorph monad-control
    mtl pretty-show primitive random resourcet stm template-haskell
    text time transformers transformers-base wl-pprint-annotated
  ];
  testHaskellDepends = [
    base containers mmorph mtl pretty-show text transformers
  ];
  homepage = "https://hedgehog.qa";
  description = "Release with confidence";
  license = lib.licenses.bsd3;
}
