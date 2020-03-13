#!/usr/bin/env bash
set -xe

installdir="tmp"

test -e "$installdir" && test -d "${installdir}" && rm -rv "${installdir}"

cabal install --installdir=${installdir} qwdeserver/
cabal --ghcjs install --installdir=${installdir} qwdeclient/

cp -rv "$(readlink tmp/qwdeclient)".jsexe tmp/
sudo cp -rva tmp/qwdeclient.jsexe/all.js  /var/www/qwde/html/static/

# to run:
# ./tmp/qwdeserver
