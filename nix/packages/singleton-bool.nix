{ mkDerivation, base, dec, lib }:
mkDerivation {
  pname = "singleton-bool";
  version = "0.1.5";
  sha256 = "405dd57dea92857c04f539c3394894c40c8103ea0c4f3f0fdbfbd8acccde899f";
  revision = "2";
  editedCabalFile = "118j0h29nqg2acqbzif2ffqnanjbwnqmv2kch9z7xiwqkz6iq8an";
  libraryHaskellDepends = [ base dec ];
  homepage = "https://github.com/phadej/singleton-bool#readme";
  description = "Type level booleans";
  license = lib.licenses.bsd3;
}
