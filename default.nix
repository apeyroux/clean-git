with import <nixpkgs> {};

let ghc = "ghc802"; in haskell.packages.${ghc}.callPackage ./clean-git.nix {}
