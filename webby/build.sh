#!/usr/bin/env bash
set -xe

# apt install ghcjs-8.4
command ghcjs 2>/dev/null || export PATH="/opt/ghcjs/8.4/bin/:/opt/ghc/bin/:${PATH}"
if [ -e "dist-newstyle" ]
then
  cabal build --ghcjs qwdeclient 
  cabal build qwdeserver 
fi


installdir="tmp"

test -e "$installdir" && test -d "${installdir}" && rm -rv "${installdir}"

cabal install --installdir=${installdir} qwdeserver/
cabal --ghcjs install --installdir=${installdir} qwdeclient/

cp -rv "$(readlink tmp/qwdeclient)".jsexe tmp/
cp -rv tmp/qwdeclient.jsexe/all.js  /var/www/qwde/html/static/

# to run:
# ./tmp/qwdeserver
