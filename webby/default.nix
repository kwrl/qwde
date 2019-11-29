{pkgs, miso}:
let
  client = pkgs.haskell.packages.ghcjs86.callCabal2nix "qwde-web" ./. { miso = miso; };
  server = pkgs.haskell.packages.ghc865.callCabal2nix "qwde-web" ./. {};
in
  pkgs.runCommand "qwde-web" { inherit client server;  } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/client.jsexe/all.js.externs \
      ${client}/bin/client.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  ''
