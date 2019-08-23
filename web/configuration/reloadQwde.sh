#!/usr/bin/env bash

set -xe

cd /home/dev/qwde/
git clean -f
git fetch --all
git reset --hard origin/master
./gradlew clean build test
sudo service qwde-webserver restart
