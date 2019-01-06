{ mkDerivation, aeson, base, stdenv, text, unordered-containers }:
mkDerivation {
  pname = "lovelace";
  version = "0.0.1";
  src = ./.;
  libraryHaskellDepends = [ aeson base text unordered-containers ];
  homepage = "http://noteed.com/lovelace";
  description = "A toy workflow system";
  license = stdenv.lib.licenses.bsd2;
}
