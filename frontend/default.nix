{ lib
, stdenvNoCC
, elm2nix
, elmPackages
, nodePackages
}:
let
  srcs = ./elm-srcs.nix;
in
stdenvNoCC.mkDerivation {
  name = "grafanix";

  src = lib.sourceByRegex ./. [
    "src(.*elm)?"
    "elm.json"
    "build.sh"
  ];

  buildInputs = [
    elmPackages.elm
    nodePackages.uglify-js
  ] ++ lib.optionals lib.inNixShell [
    elm2nix
    elmPackages.elm-format
  ];

  buildPhase = elmPackages.fetchElmDeps {
    elmPackages = import srcs;
    elmVersion = "0.19.1";
    registryDat = ./registry.dat;
  };

  installPhase = ''
    mkdir -p $out
    ${scripts/build.sh} src/Main.elm
    cp elm.min.js $out/main.js
  '';
}
