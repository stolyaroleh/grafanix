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

, static ? false
, zlib
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
  enableSharedExecutables = false;
  configureFlags =
    stdenv.lib.optionals static [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${zlib.static}/lib"
    ];

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
