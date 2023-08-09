{ lib
, d3
, bootstrap
, backend
, frontend
, stdenvNoCC
}:
stdenvNoCC.mkDerivation {
  name = "grafanix";
  version = "0.3";

  src = lib.sourceByRegex ../static [
    "drawGraph.js"
    "index.html"
    "main.css"
    "nix.png"
  ];

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/static
    cp ${d3} $out/static/d3.js
    cp ${bootstrap} $out/static/bootstrap.css
    cp * $out/static
    cp ${frontend}/main.js $out/static

    mkdir -p $out/bin
    cp ${backend}/bin/grafanix $out/bin/grafanix
  '';
}
