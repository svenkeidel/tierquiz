{ pkgs ? import <nixpkgs> {} }:

let hsEnv = pkgs.haskell.packages.ghcjs.ghcWithPackages(p: with p; [
    Cabal cabal-install mtl MonadRandom
]);

in pkgs.stdenv.mkDerivation {
  name = "tierquiz";
  version = "0.0.1";
  src = ./.;
  buildInputs = [
    hsEnv
  ];
}
