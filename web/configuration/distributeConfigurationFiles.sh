#!/usr/bin/env bash

echo Run this program as root from the directory of this file.

set -xe

cd /home/dev/qwde/web/configuration/
cp -rvi ./webhook.service ./qwde-webserver.service ./qwde-blog.service /etc/systemd/system/
cp -rvi ./qwde.no.conf /etc/apache2/sites-available/
cp -rvi ./proxy.conf /etc/apache2/apache2.conf
systemctl daemon-reload
# commented out, since password is different in server and dev-env
#cp -rvi ./hooks.json /var/webhook/
