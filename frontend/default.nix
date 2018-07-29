{ pkgs ? import <nixpkgs> {}}:
with pkgs;
stdenv.mkDerivation {
  name = "nix-deps-frontend";
  src = lib.sourceFilesBySuffices ./. [
    ".purs"
    ".json"
    ".js"
  ];
  buildInputs = [
    git
    purescript
    nodePackages.bower
    nodePackages.pulp
  ];
  phases = [ "unpackPhase" "buildPhase" ];
  buildPhase = ''
    export HOME=`pwd`
    bower install
    mkdir -p $out
    pulp build -O --to $out/main.js
  '';
}
