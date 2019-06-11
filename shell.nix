{ pkgs ? (import <nixpkgs> {}).pkgs
}:

let
  nix-hs-src = fetchGit {
    url = "https://code.devalot.com/pjones/nix-hs.git";
    rev = "0211a56b726d6ecdc45f41239e0f3fd15ba3bc08";
  };

  nix-hs = (import "${nix-hs-src}/default.nix" {inherit pkgs;});
  haskell = import ./overlay.nix { inherit pkgs; };

in

pkgs.mkShell {
  buildInputs = with pkgs; [

    # Haskell Dependencies:
    (haskell.ghcWithPackages (p: with p; [
      cabal-install
      hasktags
      hlint
      hoogle
      # cabal-dependency-licenses

    ] ++ p.iolaus-opaleye.propagatedBuildInputs))

    # Helper for incremental builds:
    nix-hs
  ];
}
