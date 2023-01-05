# An attribute set containing arguments to be supplied to each overlay.
args: 
# The nixpkgs package set.
pkgs:

pkgs.lib.composeManyExtensions (map (f: f args) [
  (import overlays/logict.nix)
  (import overlays/spectacle.nix)
]) pkgs pkgs
