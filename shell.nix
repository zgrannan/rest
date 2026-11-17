{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [
    cacert
    git
    nix
    cabal-install
    graphviz
    haskell.compiler.ghc9141
    z3
  ];

  LANG = "C.UTF-8";
}

