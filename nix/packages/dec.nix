{ mkDerivation, base, lib }:
mkDerivation {
  pname = "dec";
  version = "0.0.3";
  sha256 = "d9b8701244e77354ba7800f290c05f705994e2cc3a6c09b6c02d9c212add0bf9";
  revision = "2";
  editedCabalFile = "1v5f5yby0cld1ziqqgkcx8b50qkpviplspm82a6wl7lw28cjm0hs";
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/phadej/vec";
  description = "Decidable propositions";
  license = lib.licenses.bsd3;
}
