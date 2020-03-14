#!/usr/bin/env bash

set -xe

cd /home/dev/qwde/
git clean -f
git fetch --all
git reset --hard origin/master
sudo service qwde-webserver stop
sudo service qwde-frontend stop
./gradlew clean build test
cd "webby"
./build.sh
sudo service qwde-webserver start
sudo service qwde-frontend start
