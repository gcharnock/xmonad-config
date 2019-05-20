# { hspkgs ? (import <nixpkgs> { }).haskellPackages }:
{ hspkgs ? (import <nixpkgs> { }).haskell.packages.ghc844 }:
hspkgs.callPackage ./xmonad-config.nix { }
