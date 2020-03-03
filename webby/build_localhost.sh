#!/usr/bin/env bash
set -xe


cabal build --ghcjs qwdeclient 
cabal build qwdeserver 
cp -av ./dist-newstyle/build/x86_64-linux/ghcjs-8.4.0.1/qwdeclient-0.1.0.0/x/qwdeclient/build/qwdeclient/qwdeclient.jsexe/all.js ./dist-newstyle/build/x86_64-linux/ghc-8.0.2/qwdeserver-0.1.0.0/x/qwdeserver/build/qwdeserver/static/all.js
cp -arv ./dist-newstyle/build/x86_64-linux/ghcjs-8.4.0.1/qwdeclient-0.1.0.0/x/qwdeclient/build/qwdeclient/qwdeclient.jsexe/all.js static/

# to run:
#cabal run qwdeserver
