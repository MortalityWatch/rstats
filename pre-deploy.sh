#!/usr/bin/env sh

# Plugins now auto-installed from deployments/config.json

APP_NAME="stats-mortality-watch"

# Enable cache plugin for this app
ssh co "dokku nginx-cache:enable ${APP_NAME}"

# Clear nginx cache
ssh co "rm -rf /var/cache/nginx/*"
