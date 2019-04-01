{ haskellCompiler ? "ghc844" }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${haskellCompiler}" = pkgs.haskell.packages.${haskellCompiler}.override {
            overrides = new: old: {
              purescript = new.callPackage ./nix/purescript.nix {};
            };
          };
        };
      };

      hies = pkgs.callPackage ./nix/hies.nix {};

      haskellPkgs = haskell.packages.${haskellCompiler};
      grafanix-backend =
        haskellPkgs.callPackage ./nix/grafanix-backend.nix {};
      grafanix-frontend = pkgs.callPackage ./nix/grafanix-frontend.nix {
        inherit (haskellPkgs) purescript;
      };
      grafanix = pkgs.callPackage ./nix/grafanix.nix {};
    };
  };

  pkgs = import ./nixpkgs.nix { inherit config; };
in
  rec {
    inherit pkgs;

    backend = pkgs.grafanix-backend;
    frontend = pkgs.grafanix-frontend;
    grafanix = pkgs.grafanix;

    buildTools = with pkgs; [
      hies.hie84
      haskellPkgs.cabal2nix
      haskellPkgs.cabal-install
      haskellPkgs.hoogle
    ] ++ frontend.buildInputs;
  }
