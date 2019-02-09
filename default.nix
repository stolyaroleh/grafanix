let
  haskellCompiler = "ghc844";

  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${haskellCompiler}" = pkgs.haskell.packages.${haskellCompiler}.override {
            overrides = new: old: {
              hies = new.callPackage ./nix/hies.nix {};
              purescript = new.callPackage ./nix/purescript.nix {};
            };
          };
        };
      };

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
    backend = pkgs.grafanix-backend;
    frontend = pkgs.grafanix-frontend;
    grafanix = pkgs.grafanix;

    buildTools = with pkgs.haskellPkgs; [
      hies.hie84
      cabal2nix
      cabal-install
      hoogle
    ] ++ frontend.buildInputs;
  }