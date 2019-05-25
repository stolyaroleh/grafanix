{ mkDerivation
, stdenv

, aeson
, attoparsec
, base
, bytestring
, containers
, errors
, filepath
, hashable
, hspec
, hspec-attoparsec
, lrucaching
, optparse-applicative
, protolude
, scotty
, text
, typed-process
, vector
, wai-middleware-static
}:
mkDerivation {
  pname = "grafanix-backend";
  version = "0.1.0.0";
  src = stdenv.lib.sourceByRegex ../backend [
    "app"
    "app/.*\.hs"
    "src"
    "src/.*\.hs"
    "test"
    "test/.*\.hs"
    "test/.*\.dot"
    "grafanix\.cabal"
    "Setup\.hs"
  ];

  isExecutable = true;
  enableSharedLibraries = false;

  executableHaskellDepends = [
    aeson
    attoparsec
    base
    bytestring
    containers
    errors
    filepath
    hashable
    lrucaching
    optparse-applicative
    protolude
    scotty
    text
    typed-process
    vector
    wai-middleware-static
  ];
  testHaskellDepends = [ base hspec hspec-attoparsec ];
  license = stdenv.lib.licenses.mit;
}
