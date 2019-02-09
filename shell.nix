let
  pkgs = import ./nixpkgs.nix {};
  project = import ./.;
in
  (
    pkgs.haskell.lib.addBuildTools
      project.backend
      project.buildTools
  ).env
