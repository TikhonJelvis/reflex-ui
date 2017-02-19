# Based on the Nix-Stack-GHCJS skeleton project by Anders Papitto:
# https://github.com/anderspapitto/nix-stack-ghcjs-demo/blob/master/default.nix

let
  pkgs = import <nixpkgs> {};

  # bound against a particular commit of reflex-platform:
  reflex-platform = pkgs.fetchgit {
    url    = "git@github.com:reflex-frp/reflex-platform.git";
    rev    = "0de88ce3a17fc47353111150ba86a91159fe2cb6";
    sha256 = "1l87svhfhkhyhb27qig927d5g4ddc1x3q7whr40r2y894wsaw5vr";
  };

  reflexPkgs = import reflex-platform {};

  ghc-env = reflexPkgs.ghc.ghcWithPackages (p: with p; [
    intero
    reflex-dom
  ]);

  ghcjs-env = reflexPkgs.ghcjs.ghcWithPackages (p: with p; [
    reflex-dom
  ]);
in
  pkgs.buildEnv {
    name = "ghc-and-ghcjs-env";
    paths = [
      ghc-env
      ghcjs-env
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.stack
      pkgs.nodejs
    ];
  }
