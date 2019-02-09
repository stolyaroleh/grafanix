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
    cp ${grafanix-backend}/bin/grafanix $out

    cat <<EOF > $out/config.dhall
    { nixpkgsPath = "${<nixpkgs>}"
    , staticPath = "$out/static"
    , duCacheSize = +2048
    , whyCacheSize = +512
    }
    EOF
  ''
