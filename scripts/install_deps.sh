#!/bin/bash

set -e 

sudo apt-get update
# sudo apt-get install stack 
# TODO: add all stack/libC deps

# --------------- INSTALL MYSQL AND DEPENDENCIES -------------------
sudo apt-get install libmysqlclient-dev libpcre3-dev mysql-server

sudo mysql -e "CREATE USER IF NOT EXISTS 'llamabot'@'localhost' IDENTIFIED BY 'llama'"
sudo mysql -e "CREATE DATABASE IF NOT EXISTS llamabot"

# ---------------- INSTALL NGINX AND DEPENDENCIES ------------------
sudo apt-get install nginx
sudo nginx -v

sudo cp ../nginx.conf /etc/nginx/.

sudo nginx -s reload

# ------------------ INSTALL HASKELL STACK -----------------------
curl -sSL https://get.haskellstack.org/ | sh
