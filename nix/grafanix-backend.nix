{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, dhall, errors, hashable, hspec, hspec-attoparsec, lrucaching, protolude, scotty
, stdenv, text, typed-process, wai-middleware-static
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
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers dhall errors hashable
    lrucaching protolude scotty text typed-process
    wai-middleware-static
  ];
  testHaskellDepends = [ base hspec hspec-attoparsec ];
  license = stdenv.lib.licenses.mit;
}
