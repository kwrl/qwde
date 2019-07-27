#!/usr/bin/env bash

set -xe

cd ./qwde/
git clean -f
git fetch --all
git reset --hard origin/master
mvn test
sudo service qwde-webserver restart
