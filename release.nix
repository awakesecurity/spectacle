let
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
      haskellPackages = oldPkgs.haskellPackages.extend (
        newPkgs.lib.fold newPkgs.lib.composeExtensions (_: _: {}) [
          (haskellPackagesNew: haskellPackagesOld: {
            spectacle = haskellPackagesNew.callCabal2nix "spectacle" ./. { };
          })
        ]
      );
    })
  ];

  pkgs = import ./nix/nixpkgs.nix { inherit overlays; };

  spectacle-shell = (pkgs.haskellPackages.shellFor {
    packages = p: [
      p.spectacle
    ];
    buildInputs = [ pkgs.haskellPackages.cabal-install pkgs.haskellPackages.ghcid ];
  });

  fourmoluOptions =
    let
      options = import ./fourmolu.nix;
      format = x: if builtins.isBool x then (if x then "true" else "false") else (if builtins.isString x then x else toString x);
      opts = pkgs.lib.mapAttrsToList (k: v: "--${k} ${format v}") options;
    in pkgs.lib.concatStringsSep " " opts;

in {
  inherit pkgs fourmoluOptions;
  inherit (pkgs.haskellPackages) fourmolu dhall-yaml dhall-nix;
  inherit spectacle-shell;

  spectacle = pkgs.haskell.lib.appendConfigureFlags pkgs.haskellPackages.spectacle [
    "--ghc-options=-Werror"
    "--ghc-options=-Wmissing-import-lists"
    "--ghc-options=-Wunused-packages"
  ];
}
