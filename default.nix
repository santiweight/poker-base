{ mkDerivation, base, containers, deriving-compat, hpack, lens
, prettyprinter, QuickCheck, stdenv, tasty, tasty-discover
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "poker-base";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers deriving-compat lens prettyprinter QuickCheck tasty
    tasty-discover tasty-quickcheck text time
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base containers deriving-compat lens prettyprinter QuickCheck tasty
    tasty-discover tasty-quickcheck text time
  ];
  testToolDepends = [ tasty-discover ];
  prePatch = "hpack";
  homepage = "https://github.com/santiweight/Exploit#readme";
  license = stdenv.lib.licenses.bsd3;
}
