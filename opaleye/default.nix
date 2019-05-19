{ pkgs    ? import <nixpkgs> {}
, haskell ? pkgs.haskellPackages
}:

haskell.callPackage ./iolaus-opaleye.nix { }
