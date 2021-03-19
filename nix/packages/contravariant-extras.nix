{ mkDerivation, base, contravariant, lib, template-haskell
, template-haskell-compat-v0208
}:
mkDerivation {
  pname = "contravariant-extras";
  version = "0.3.5.2";
  sha256 = "e9cb90b7ede2d491c8bb2a9d44ab151c8a5d89d35a74703d38488a94c0fb7c46";
  libraryHaskellDepends = [
    base contravariant template-haskell template-haskell-compat-v0208
  ];
  homepage = "https://github.com/nikita-volkov/contravariant-extras";
  description = "Extras for the \"contravariant\" package";
  license = lib.licenses.mit;
}
