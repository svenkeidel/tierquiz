{ pkgs ? import <nixpkgs> {} }:

let hsEnv = pkgs.haskellPackages.ghcWithPackages(p: with p; [
    Cabal cabal-install mtl text aeson MonadRandom
  ]);

in pkgs.stdenv.mkDerivation {
  name = "tierquiz";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
