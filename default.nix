{}:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { };

  haskellCompiler = "ghc927";
  haskellPackages = pkgs.haskell.packages.${haskellCompiler};

  static = import (sources.static-haskell-nix + "/survey") {

    compiler = haskellCompiler;
    integer-simple = false;
  };
  staticHaskellPackages = static.haskellPackagesWithLibsReadyForStaticLinking;

  d3 = builtins.fetchurl {
    url = "https://d3js.org/d3.v5.min.js";
    sha256 = "0g5529s28dm27sqp5zzff1ipva1fyipdswl51c7h3ps7715r5gjx";
  };

  bootstrap = builtins.fetchurl {
    url = "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css";
    sha256 = "0dldiln2s3z8iqc5ccjid2i5gh9527naas064bwly8x9lrfrxcb0";
  };
in
rec {
  backend = haskellPackages.callPackage ./nix/grafanix-backend.nix {
    static = false;
  };
  backend-static = staticHaskellPackages.callPackage ./nix/grafanix-backend.nix {
    static = true;
    ncurses = static.pkgs.ncurses.override {
      enableStatic = true;
    };
    zlib = static.pkgs.zlib;
  };

  frontend = pkgs.callPackage ./frontend/default.nix { };

  grafanix = pkgs.callPackage ./nix/grafanix.nix {
    inherit bootstrap d3;
    inherit backend frontend;
  };
  grafanix-static = pkgs.callPackage ./nix/grafanix.nix {
    inherit bootstrap d3;
    inherit frontend;
    backend = backend-static;
  };

  grafanix-release = pkgs.callPackage ./nix/grafanix-release.nix {
    grafanix = grafanix-static;
  };

  shell = (
    pkgs.haskell.lib.addBuildTools
      backend
      (
        [
          haskellPackages.cabal2nix
          haskellPackages.cabal-install
          haskellPackages.ghcide
          haskellPackages.ormolu
          pkgs.elm2nix
          pkgs.inotify-tools
          pkgs.niv
          pkgs.treefmt
        ] ++ frontend.buildInputs
      )
  ).env.overrideAttrs (
    old: {
      shellHook = ''
        ln -snfv ${bootstrap} ${builtins.toString ./.}/static/bootstrap.css
        ln -snfv ${d3} ${builtins.toString ./.}/static/d3.js
      '';
    }
  );
}
