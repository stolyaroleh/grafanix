{ pkgs ? import <nixpkgs> {}}:
with pkgs;
stdenv.mkDerivation {
  name = "grafanix-static";
  src = lib.sourceFilesBySuffices ./. [
    "index.html"
    "main.css"
    "parrot.gif"
    "d3.js"
  ];
  installPhase = ''
    mkdir -p $out
    cp * $out
  '';
}