{ pkgs ? import <nixpkgs> {} }:
let
  backend = import ./backend { inherit pkgs; };
  frontend = import ./frontend { inherit pkgs; };
  static = import ./static { inherit pkgs; };
in
  with pkgs;
  runCommand "grafanix" {} ''
    mkdir -p $out/static
    cp -r ${static}/* $out/static
    cp ${frontend}/main.js $out/static
    cp ${backend}/bin/main $out
    echo '{ nixpkgsPath = "${<nixpkgs>}"' >> $out/config.dhall
    echo ", staticPath = \"$out/static\"" >> $out/config.dhall
    echo ', duCacheSize = 2048' >> $out/config.dhall
    echo ', whyCacheSize = 512' >> $out/config.dhall
    echo '}' >> $out/config.dhall
  ''
