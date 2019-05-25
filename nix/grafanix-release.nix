{ runCommand
, grafanix
, zip
}:
let
  version = grafanix.version;
in
  runCommand "grafanix-${version}.zip" {} ''
    cd ${grafanix}
    ${zip}/bin/zip -9 -r $out *
  ''