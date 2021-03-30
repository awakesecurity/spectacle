pkgsFinal: pkgsPrev:

{
  haskellPackages = pkgsPrev.haskellPackages.override (old: {
    overrides =
      pkgsPrev.lib.composeExtensions
        (old.overrides or (_: _: {}))
        (haskellPackagesFinal: haskellPackagesPrev: {
          spectacle = haskellPackagesPrev.callCabal2nix "spectacle" ../. { };
        });
  });
}
