{ nixpkgs ? import <nixpkgs> {}
, ghc ? nixpkgs.ghc
, polysemy
, polysemy-plugin
}:

with nixpkgs;

nixpkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "exploit-poker";
  buildInputs = [ zlib postgresql ];
  src = ./.;
}
