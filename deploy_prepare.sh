#!/usr/bin/env sh

ssh co "dokku plugin:install https://github.com/USMortality/dokku-nginx-cache nginx-cache"
ssh co "dokku nginx-cache:enable ${APP_NAME}"
