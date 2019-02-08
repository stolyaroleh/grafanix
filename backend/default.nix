let
  compiler = "ghc844";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages.${compiler}.override {
            overrides = new: old:
              with builtins;
              let
                dropNixExtension = replaceStrings [".nix"] [""];
                nixFiles = attrNames (readDir ./nix);
                packageNames = map dropNixExtension nixFiles;
                toPackage = name:
                  new.callPackage (./nix + "/${name}.nix") {};
              in
                pkgs.lib.genAttrs packageNames toPackage;
          };
        };
      };
    };
  };

  pkgs = import ../nixpkgs.nix { inherit config; };
  haskellPkgs = pkgs.haskell.packages.${compiler};
  haskellLib = pkgs.haskell.lib;

  # Haskell Ide Engine
  # Pass pkgs to the function since
  # hie it must use the same ghc
  hies = import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
    sha256 = "1px146agwmsi0nznc1zd9zmhgjczz6zlb5yf21sp4mixzzbjsasq";
  }) {
    inherit pkgs;
  };
in
  haskellLib.addBuildTools haskellPkgs.grafanix (
    if pkgs.lib.inNixShell
      then [
        hies.hie84
        haskellPkgs.cabal2nix
        haskellPkgs.cabal-install
        haskellPkgs.hoogle
      ]
      else []
  )
