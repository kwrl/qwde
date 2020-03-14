#!/usr/bin/env bash

set -xe

cd /home/dev/qwde/
git clean -f
git fetch --all
git reset --hard origin/master
./gradlew clean build test
cd "webby"
./build.sh
sudo service qwde-webserver restart
sudo service qwde-frontend restart
