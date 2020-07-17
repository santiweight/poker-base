{ mkDerivation, aeson, alex, array, async, attoparsec, base
, base-compat, bytestring, comonad, containers, data-default
, deepseq, deriving-compat, directory, do-notation, either, errors
, esqueleto, filepath, free, generic-arbitrary, generic-data, happy
, hpack, http-media, HUnit, lens, lucid, megaparsec, monad-logger
, monad-loops, mtl, optparse-applicative, persistent
, persistent-postgresql, persistent-template, polysemy
, polysemy-plugin, prettyprinter, QuickCheck, quickcheck-assertions
, random-shuffle, recursion-schemes, resourcet, scientific
, serialise, servant, servant-lucid, servant-server, singletons
, sort, stdenv, string-conversions, syb, tasty, tasty-discover
, tasty-hspec, tasty-hunit, tasty-quickcheck, text, time
, transformers, uniplate, unordered-containers, vector, wai, warp
}:
mkDerivation {
  pname = "exploit-poker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson array async attoparsec base base-compat bytestring comonad
    containers data-default deepseq deriving-compat directory
    do-notation either errors esqueleto filepath free generic-arbitrary
    generic-data http-media HUnit lens lucid megaparsec monad-logger
    monad-loops mtl optparse-applicative persistent
    persistent-postgresql persistent-template polysemy polysemy-plugin
    prettyprinter QuickCheck quickcheck-assertions random-shuffle
    recursion-schemes resourcet scientific serialise servant
    servant-lucid servant-server singletons sort string-conversions syb
    tasty tasty-discover tasty-hspec tasty-hunit tasty-quickcheck text
    time transformers uniplate unordered-containers vector wai warp
  ];
  libraryToolDepends = [ alex happy hpack ];
  executableHaskellDepends = [
    aeson array async attoparsec base base-compat bytestring comonad
    containers data-default deepseq deriving-compat directory
    do-notation either errors esqueleto filepath free generic-arbitrary
    generic-data http-media HUnit lens lucid megaparsec monad-logger
    monad-loops mtl optparse-applicative persistent
    persistent-postgresql persistent-template polysemy polysemy-plugin
    prettyprinter QuickCheck quickcheck-assertions random-shuffle
    recursion-schemes resourcet scientific serialise servant
    servant-lucid servant-server singletons sort string-conversions syb
    tasty tasty-discover tasty-hspec tasty-hunit tasty-quickcheck text
    time transformers uniplate unordered-containers vector wai warp
  ];
  testHaskellDepends = [
    aeson array async attoparsec base base-compat bytestring comonad
    containers data-default deepseq deriving-compat directory
    do-notation either errors esqueleto filepath free generic-arbitrary
    generic-data http-media HUnit lens lucid megaparsec monad-logger
    monad-loops mtl optparse-applicative persistent
    persistent-postgresql persistent-template polysemy polysemy-plugin
    prettyprinter QuickCheck quickcheck-assertions random-shuffle
    recursion-schemes resourcet scientific serialise servant
    servant-lucid servant-server singletons sort string-conversions syb
    tasty tasty-discover tasty-hspec tasty-hunit tasty-quickcheck text
    time transformers uniplate unordered-containers vector wai warp
  ];
  testToolDepends = [ tasty-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/santiweight/Exploit#readme";
  license = stdenv.lib.licenses.bsd3;
}
