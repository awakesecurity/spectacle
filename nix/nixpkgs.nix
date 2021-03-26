args:

let
  # master on 2021-03-25
  rev = "8eb359be153cbefcffc8408bd30bcd369bd5d793";
  sha256 = "16v7byw8b0grw4bkmpa6zww021wld23h05rkch2vw3i92xrbg9p7";

  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };

in
  import nixpkgs ({ config = {}; } // args)
