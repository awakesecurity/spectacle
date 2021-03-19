{ mkDerivation, base, base16-bytestring, binary, bytestring
, containers, directory, filepath, lib, tasty, tasty-hunit
}:
mkDerivation {
  pname = "resolv";
  version = "0.1.2.0";
  sha256 = "81a2bafad484db123cf8d17a02d98bb388a127fd0f822fa022589468a0e64671";
  revision = "1";
  editedCabalFile = "19pm4cg28viyl7r0viz4wfmm9w4nqxkjlb6kknfqcajjqmdacqad";
  libraryHaskellDepends = [
    base base16-bytestring binary bytestring containers
  ];
  testHaskellDepends = [
    base bytestring directory filepath tasty tasty-hunit
  ];
  description = "Domain Name Service (DNS) lookup via the libresolv standard library routines";
  license = lib.licenses.gpl2Plus;
}
