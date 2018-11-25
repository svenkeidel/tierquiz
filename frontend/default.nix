{ pkgs ? import <nixpkgs> {} }:

in pkgs.stdenv.mkDerivation {
  name = "tierquiz-frontend";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    pkgs.nodejs
  ];
}
