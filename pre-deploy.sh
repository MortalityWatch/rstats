#!/usr/bin/env sh

APP_NAME="stats-mortality-watch"

# Enable CORS plugin for this app
ssh co "dokku nginx-cors:enable --no-restart ${APP_NAME}"
