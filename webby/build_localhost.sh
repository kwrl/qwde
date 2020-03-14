#!/usr/bin/env bash

# apt install ghcjs-8.4
command ghcjs 2>/dev/null || export PATH="/opt/ghcjs/8.4/bin/:/opt/ghc/bin/:${PATH}"
if [ -e "dist-newstyle" ]
then
  cabal build --ghcjs qwdeclient 
  cabal build qwdeserver 
fi

serverdir="$(find dist-newstyle/ -name qwdeserver -type f | head -n1)"
serverdir="$(dirname $serverdir)"
mkdir "${serverdir}"/static static

set -xe
cabal build --ghcjs qwdeclient 
cabal build qwdeserver 


clientfile="$(find dist-newstyle/ -name all.js -and -regex ".*qwdeclient.*" | head -n1)"
cp -av "${clientfile}" "${serverdir}"/static
cp -arv "${clientfile}" static/

# to run:
#cabal run qwdeserver
