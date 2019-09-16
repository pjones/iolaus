{ pkgs ? import <nixpkgs> { }
}:

{
  iolaus-opaleye = import ./opaleye { inherit pkgs; };
  iolaus-crypto  = import ./crypto  { inherit pkgs; };
}
