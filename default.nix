{ mkDerivation, aeson, base, stdenv, tasty, tasty-hunit
, tasty-quickcheck, tasty-smallcheck, text, unordered-containers
}:
mkDerivation {
  pname = "lovelace";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ aeson base text unordered-containers ];
  testHaskellDepends = [
    base tasty tasty-hunit tasty-quickcheck tasty-smallcheck
  ];
  homepage = "http://noteed.com/lovelace";
  description = "A toy workflow system";
  license = stdenv.lib.licenses.bsd2;
}
