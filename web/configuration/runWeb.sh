#!/usr/bin/env bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
XDG_DATA_HOME=$SCRIPTPATH/../../dataprovider \
  java -Xmx4096M -jar $SCRIPTPATH/../build/libs/shadow-0.0.1-all.jar
