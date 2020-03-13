#!/usr/bin/env bash
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
