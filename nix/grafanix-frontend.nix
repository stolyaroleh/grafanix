{ cacert
, git
, lib
, nodePackages
, purescript
, stdenvNoCC
}:
stdenvNoCC.mkDerivation {
  name = "grafanix-frontend";
  src = lib.sourceByRegex ../frontend [
    "src"
    "src/.*\.purs"
    "src/.*\.js"
    "bower\.json"
  ];
  buildInputs = [
    git
    purescript
    nodePackages.bower
    nodePackages.pulp
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    # Hacks to make bower work in `nix-build`
    export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    export HOME=.

    bower install
    mkdir -p $out
    pulp build -O --to $out/main.js
  '';
}
