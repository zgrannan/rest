{ pkgs ? import ./nixpkgs.nix {} }:

with pkgs;

mkShell {
  buildInputs = [
    cacert
    git
    nix
    stack
  ];
}

