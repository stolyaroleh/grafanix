{ grafanix-backend
, grafanix-frontend
, runCommand
}:
  runCommand "grafanix" {} ''
    mkdir -p $out/static
    pushd ${../static}
    cp index.html \
       main.css   \
       parrot.gif \
       d3.js      \
       $out/static
    popd
    cp ${grafanix-frontend}/main.js $out/static

    mkdir -p $out/bin
    cp ${grafanix-backend}/bin/grafanix $out/bin/grafanix
  ''
