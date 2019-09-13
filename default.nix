{ pkgs ? import <nixpkgs> { }
}:

{
  iolaus-opaleye = import ./opaleye { inherit pkgs; };
}
