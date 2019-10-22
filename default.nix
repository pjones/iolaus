{ pkgs ? import <nixpkgs> { }
}:

{
  iolaus-crypto     = import ./crypto     { inherit pkgs; };
  iolaus-opaleye    = import ./opaleye    { inherit pkgs; };
  iolaus-validation = import ./validation { inherit pkgs; };
}
