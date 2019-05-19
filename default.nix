{ pkgs    ? import <nixpkgs> { }
, haskell ? pkgs.haskellPackages
}:

# FIXME:
import ./opaleye { inherit pkgs haskell; }
