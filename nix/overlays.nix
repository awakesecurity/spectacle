# An attribute set containing arguments to be supplied to each overlay.
{ ghcVersion }: 
# The nixpkgs package set.
pkgs:

pkgs.lib.composeManyExtensions [
  (import overlays/haskell.nix {
    inherit ghcVersion;
  })
] pkgs pkgs
