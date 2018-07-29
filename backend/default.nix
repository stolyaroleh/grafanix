{ pkgs ? import <nixpkgs> {}}:
let
  haskellPackages = pkgs.haskell.packages.ghc822;

  # Haskell Ide Engine
  # Pass pkgs to the function since
  # hie it must use the same ghc
  hies = import (pkgs.fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "8f04568aa8c3215f543250eb7a1acfa0cf2d24ed";
    sha256 = "06ygnywfnp6da0mcy4hq0xcvaaap1w3di2midv1w9b9miam8hdrn";
  }) {
    inherit pkgs;
  };

  # hfmt does not pass its own unit tests right now :(
  # I'll fix and update this bit later
  hfmt = haskellPackages.hfmt.overrideAttrs (
    oldAttrs: {
      doCheck = false;
    }
  );

  devInputs = [
    hies.hie82
    hfmt
  ];
in
  with pkgs;
  pkgs.haskell.lib.buildStackProject {
    name = "nix-deps";
    src = lib.sourceFilesBySuffices ./. [
      ".hs"
      "grafanix.cabal"
      "stack.yaml"
    ];
    configurePhase = ''
      export STACK_ROOT=$NIX_BUILD_TOP/.stack
      for pkg in ''${pkgsHostHost[@]} ''${pkgsHostBuild[@]} ''${pkgsHostTarget[@]}
      do
        [ -d "$pkg/lib" ] && \
          export STACK_IN_NIX_EXTRA_ARGS+=" --extra-lib-dirs=$pkg/lib"
        [ -d "$pkg/include" ] && \
          export STACK_IN_NIX_EXTRA_ARGS+=" --extra-include-dirs=$pkg/include"
      done
      true
    '';
    buildInputs = [ re2 zlib ] ++ 
      lib.optionals lib.inNixShell devInputs;
  }
