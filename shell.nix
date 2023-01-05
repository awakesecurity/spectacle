{ ghc ? "ghc924" }:

# This retrieves the default develop shell from `flake.nix`. This is only needed 
# for backward compatibility with Haskell tooling that doesn't support flakes 
# directly yet. 
let 
  flake = builtins.getFlake ("git+file://" + toString ./.);
  system = builtins.currentSystem;
in flake.devShells."${system}".default