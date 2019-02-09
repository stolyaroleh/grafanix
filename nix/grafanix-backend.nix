{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, dhall, errors, hashable, lrucaching, protolude, re2, scotty
, stdenv, text, typed-process, wai-middleware-static
}:
mkDerivation {
  pname = "grafanix-backend";
  version = "0.1.0.0";
  src = stdenv.lib.sourceByRegex ../backend [
    "src"
    "src/.*\.hs"
    "grafanix\.cabal"
    "Setup\.hs"
  ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base bytestring containers dhall errors hashable
    lrucaching protolude re2 scotty text typed-process
    wai-middleware-static
  ];
  license = stdenv.lib.licenses.mit;
}
