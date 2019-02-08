let
  bootstrap = import <nixpkgs> {};
  nixpkgs = builtins.fromJSON (builtins.readFile ./nixpkgs.json);
in
  import (bootstrap.fetchFromGitHub nixpkgs)

