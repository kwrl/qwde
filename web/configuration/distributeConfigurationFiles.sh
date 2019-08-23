#!/usr/bin/env bash

echo Run this program as root from the directory of this file.

set -xe

cp -rvi ./webhook.service ./qwde-webserver.service /etc/systemd/system/
systemctl daemon-reload
# commented out, since password is different in server and dev-env
#cp -rvi ./hooks.json /var/webhook/
