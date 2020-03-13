{pkgs ? import <nixpkgs> {} }:
let
  clientutil = pkgs.haskell.packages.ghcjs86.callCabal2nix "qwdeutil" ./qwdeutil {
  };
  clientshared = pkgs.haskell.packages.ghcjs86.callCabal2nix "qwdeshared" ./qwdeshared {
    qwdeutil = clientutil;
  };
  client = pkgs.haskell.packages.ghcjs86.callCabal2nix "qwdeclient" ./qwdeclient {
    qwdeutil = clientutil;
    qwdeshared = clientshared;
  };

  serverutil = pkgs.haskell.packages.ghc865.callCabal2nix "qwdeutil" ./qwdeutil {
  };
  servershared = pkgs.haskell.packages.ghc865.callCabal2nix "qwdeshared" ./qwdeshared {
    qwdeutil = serverutil;
  };
  server = pkgs.haskell.packages.ghc865.callCabal2nix "qwdeserver" ./qwdeserver {
    qwdeutil = serverutil;
    qwdeshared = servershared;
  };
in
  pkgs.runCommand "qwde-webserver" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    ${pkgs.closurecompiler}/bin/closure-compiler --compilation_level ADVANCED_OPTIMIZATIONS \
      --jscomp_off=checkVars \
      --externs=${client}/bin/qwdeclient.jsexe/all.js.externs \
      ${client}/bin/qwdeclient.jsexe/all.js > temp.js
    mv temp.js $out/static/all.js
  ''
