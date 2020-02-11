{}:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  static =
    import (sources.static-haskell-nix + "/survey") {
      integer-simple = true;
    };
  haskellCompiler = "ghc864";
  staticHaskellPackages = static.haskellPackagesWithLibsReadyForStaticLinking;
  niv = (import sources.niv { inherit pkgs; }).niv;
  hie = (import sources.all-hies {}).versions."${haskellCompiler}";
  websocat = (import sources.unstable {}).websocat;

  d3 = sources.d3;

  bootstrap = sources.bootstrap;
in
  rec {
    backend = pkgs.haskellPackages.callPackage ./nix/grafanix-backend.nix {};
    backend-static = staticHaskellPackages.callPackage ./nix/grafanix-backend.nix {
      static = true;
      zlib = static.pkgs.zlib;
    };

    frontend = pkgs.callPackage ./frontend {};

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
            hie
            pkgs.haskellPackages.cabal2nix
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.hoogle
            pkgs.haskellPackages.ghcid
            pkgs.haskellPackages.stack
            niv
            websocat
          ] ++
          frontend.buildInputs
        )
    ).env.overrideAttrs (
      old: {
        shellHook = ''
          (
            cd ${builtins.toString ./static}
            ln -snf ${bootstrap} bootstrap.css
            ln -snf ${d3} d3.js
          )
        '';
      }
    );
  }
