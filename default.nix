{ pkgs    ? import <nixpkgs> { }
, haskell ? pkgs.haskellPackages
}:

let
  overlay = import ./overlay.nix { inherit pkgs haskell; };
in
{
  iolaus-opaleye = overlay.iolaus-opaleye;
}
