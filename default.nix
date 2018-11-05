# { hspkgs ? (import <nixpkgs> { }).haskellPackages }:
{ hspkgs ? (import <nixpkgs> { }).haskell.packages.ghc843 }:
hspkgs.callPackage ./xmonad-config.nix { }
