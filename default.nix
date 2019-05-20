{ haskellCompiler ? "ghc864" }:
let
  config = {
    packageOverrides = pkgs: rec {
      grafanix-backend =
        pkgs.haskell.packages.${haskellCompiler}.callPackage ./nix/grafanix-backend.nix {};
      grafanix-frontend = pkgs.callPackage ./nix/grafanix-frontend.nix {
        purescript = easyPS.inputs.purs;
      };
      grafanix = pkgs.callPackage ./nix/grafanix.nix {};
    };
  };

  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; };
  niv = (import sources.niv { inherit pkgs; }).niv;
  hie = (import sources.all-hies {}).versions."${haskellCompiler}";
  easyPS = import sources.easy-purescript-nix;
in
  rec {
    backend = pkgs.grafanix-backend;
    frontend = pkgs.grafanix-frontend;
    grafanix = pkgs.grafanix;

    buildTools = with pkgs; [
      hie
      haskellPackages.cabal2nix
      haskellPackages.cabal-install
      haskellPackages.hoogle
      niv
    ] ++ frontend.buildInputs;

    shell =
      pkgs.haskell.lib.addBuildTools
      backend
      buildTools;
  }
