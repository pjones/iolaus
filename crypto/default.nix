{ pkgs ? import <nixpkgs> { }
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/open/nix-hs.git";
    rev = "a2b666faf8cb3c6f769655dfb36f4695f78bc3c3";
  };

  nix-hs = import "${nix-hs-src}/default.nix" { inherit pkgs; };

in nix-hs {
  cabal = ./iolaus-crypto.cabal;

  overrides = lib: self: super: with lib; {
    iolaus-opaleye = import ../opaleye { inherit pkgs; };

    zxcvbn-hs = lib.fetchGit {
      url = "https://code.devalot.com/sthenauth/zxcvbn-hs.git";
      rev = "7c05b0c91b4b7f98777cf83ba5b24cdc1d62bfcd";
      ref = "next";
    };
  };
}
