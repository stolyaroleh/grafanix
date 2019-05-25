{ backend
, frontend
, runCommand
}:
  runCommand "grafanix" {
    version = "0.1";
  } ''
    mkdir -p $out/static
    pushd ${../static}
    cp index.html \
       main.css   \
       parrot.gif \
       d3.js      \
       $out/static
    popd
    cp ${frontend}/main.js $out/static

    mkdir -p $out/bin
    cp ${backend}/bin/grafanix $out/bin/grafanix
  ''
