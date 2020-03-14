#!/usr/bin/env bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
cd "${SCRIPTPATH}/../../webby/"
/opt/cabal/bin/cabal run qwdeserver
