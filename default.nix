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
  easyPS = import sources.easy-purescript-nix;
in
  rec {
    backend = pkgs.haskellPackages.callPackage ./nix/grafanix-backend.nix {};
    backend-static = staticHaskellPackages.callPackage ./nix/grafanix-backend.nix {
      static = true;
      zlib = static.pkgs.zlib;
    };

    frontend = pkgs.callPackage ./nix/grafanix-frontend.nix {
      purescript = easyPS.inputs.purs;
    };

    grafanix = pkgs.callPackage ./nix/grafanix.nix {
      inherit backend frontend;
    };
    grafanix-static = pkgs.callPackage ./nix/grafanix.nix {
      inherit frontend;
      backend = backend-static;
    };

    buildTools = [
      hie
      pkgs.haskellPackages.cabal2nix
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.hoogle
      niv
    ] ++ frontend.buildInputs;

    shell = (
      pkgs.haskell.lib.addBuildTools
        backend
        buildTools
    ).env;
  }
