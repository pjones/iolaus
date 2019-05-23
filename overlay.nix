{ pkgs    ? import <nixpkgs> { }
, haskell ? pkgs.haskellPackages
}:

let
  overrides = self: super: {
    iolaus-opaleye = self.callPackage ./opaleye/iolaus-opaleye.nix { };
  };

in
  haskell.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: {})) overrides; })
