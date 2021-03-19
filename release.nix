let
  config = { };

  readDirectory = directory:
    haskellPackagesNew: haskellPackagesOld:
      let
        haskellPaths = builtins.attrNames (builtins.readDir directory);

        toKeyVal = file: {
          name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;

          value = haskellPackagesNew.callPackage (directory + "/${file}") { };
        };

      in
    builtins.listToAttrs (map toKeyVal haskellPaths);


  overlays = [
    (newPkgs: oldPkgs: rec {
      haskellPackagesGhc8101 = oldPkgs.haskell.packages.ghc8101.extend (
        newPkgs.lib.fold newPkgs.lib.composeExtensions (_: _: {}) [
          (readDirectory ./nix/packages)
          (haskellPackagesNew: haskellPackagesOld: {
            spectacle = haskellPackagesNew.callCabal2nix "spectacle" ./. { };

            # Infinite recursion since quickcheck depends on splitmix
            splitmix = newPkgs.haskell.lib.dontCheck haskellPackagesOld.splitmix;

            # HsYAML-aeson is no longer broken
            HsYAML-aeson = newPkgs.haskell.lib.unmarkBroken haskellPackagesOld.HsYAML-aeson;

            HsYAML = newPkgs.haskell.lib.dontCheck haskellPackagesOld.HsYAML;

            cassava = newPkgs.haskell.lib.dontCheck haskellPackagesOld.cassava;

            # Diff's test suite fails to compile
            Diff = newPkgs.haskell.lib.dontCheck haskellPackagesOld.Diff;

            ghc-lib-parser = newPkgs.haskell.lib.dontHaddock haskellPackagesOld.ghc-lib-parser;

            ghc-lib-parser_8_8_2_20200205 = null;
            ghc-lib-parser-ex_8_8_4_0 = null;

            primitive = newPkgs.haskell.lib.dontCheck haskellPackagesOld.primitive;

            system-fileio = newPkgs.haskell.lib.dontCheck haskellPackagesOld.system-fileio;

            psqueues = newPkgs.haskell.lib.dontCheck haskellPackagesOld.psqueues;

            HTTP = newPkgs.haskell.lib.dontCheck haskellPackagesOld.HTTP;

            ed25519 = newPkgs.haskell.lib.dontCheck haskellPackagesOld.ed25519;

            lukko = newPkgs.haskell.lib.dontCheck haskellPackagesOld.lukko;

            hackage-security = newPkgs.haskell.lib.dontCheck haskellPackagesOld.hackage-security;

            cryptohash-sha256 =
              let
                pkg = haskellPackagesNew.callPackage ./nix/packages/cryptohash-sha256.nix { cryptohash-sha256-pure = null; };
              in
                newPkgs.haskell.lib.enableCabalFlag (newPkgs.haskell.lib.dontCheck pkg) "use-cbits";

            cryptohash-sha512 = newPkgs.haskell.lib.dontCheck haskellPackagesOld.cryptohash-sha512;

            # From https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/haskell-modules/configuration-ghc-8.10.x.nix#L80
            language-haskell-extract = newPkgs.haskell.lib.appendPatch (newPkgs.haskell.lib.doJailbreak haskellPackagesOld.language-haskell-extract) (newPkgs.fetchpatch {
              name = "language-haskell-extract-0.2.4.patch";
              url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/e48738ee1be774507887a90a0d67ad1319456afc/patches/language-haskell-extract-0.2.4.patch?inline=false";
              sha256 = "0rgzrq0513nlc1vw7nw4km4bcwn4ivxcgi33jly4a7n3c1r32v1f";
            });
          })

        ]
      );
    })
  ];

  nixpkgs = import ./nix/20_03.nix;
  pkgs    = import nixpkgs { inherit config overlays; };

  spectacle-shell = (pkgs.haskellPackagesGhc8101.shellFor {
    packages = p: [
      p.spectacle
    ];
    buildInputs = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackagesGhc8101.ghcid ];
  });

  fourmoluOptions =
    let
      options = import ./fourmolu.nix;
      format = x: if builtins.isBool x then (if x then "true" else "false") else (if builtins.isString x then x else toString x);
      opts = pkgs.lib.mapAttrsToList (k: v: "--${k} ${format v}") options;
    in pkgs.lib.concatStringsSep " " opts;

in {
  inherit pkgs fourmoluOptions;
  inherit (pkgs.haskellPackagesGhc8101) fourmolu dhall-yaml dhall-nix;
  inherit spectacle-shell;

  spectacle = pkgs.haskell.lib.appendConfigureFlags pkgs.haskellPackagesGhc8101.spectacle [
    "--ghc-options=-Werror"
    "--ghc-options=-Wmissing-import-lists"
    "--ghc-options=-Wunused-packages"
  ];
}
