{ mkDerivation
, lib

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
, lrucache
, optparse-applicative
, protolude
, scotty
, text
, typed-process
, vector
, wai-cors
, wai-middleware-static

, static ? false
, ncurses ? null
, zlib ? null
}:
mkDerivation {
  pname = "grafanix-backend";
  version = "0.1.0.0";
  src = lib.sourceByRegex ../backend [
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
    lib.optionals static [
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${zlib.static}/lib:${ncurses}/lib"
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
    lrucache
    optparse-applicative
    protolude
    scotty
    text
    typed-process
    vector
    wai-cors
    wai-middleware-static
  ];
  testHaskellDepends = [ base hspec hspec-attoparsec ];
  license = lib.licenses.mit;
}
