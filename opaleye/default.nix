{ pkgs  ? import <nixpkgs> { }
, debug ? false
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "aebbf6abe46fa844e3a6511162203afb6ae339fa";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./iolaus-opaleye.cabal;
  flags = pkgs.lib.optional debug "debug";

  overrides = lib: self: super:
    (import ../nix/overrides.nix lib self super) // (with lib; {
    });
}
