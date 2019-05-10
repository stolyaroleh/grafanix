{ fetchFromGitHub }:
let
  hies = fetchFromGitHub {
    owner = "domenkozar";
    repo = "hie-nix";
    rev = "19f47e0bf2e2f1a793bf87d64bf8266062f422b1";
    sha256 = "1px146agwmsi0nznc1zd9zmhgjczz6zlb5yf21sp4mixzzbjsasq";
  };
in
  import hies {}
