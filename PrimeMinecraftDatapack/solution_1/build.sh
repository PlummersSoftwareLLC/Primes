#!/bin/bash

# This script assumes that OpenJDK 17.0.1 (or newer) is already installed
# somewhere in $PATH.

sudo apt-get update
sudo apt-get install -y wget build-essential

mkdir env
cd env

wget https://launcher.mojang.com/v1/objects/a16d67e5807f57fc4e550299cf20226194497dc2/server.jar
mkdir -p world/datapacks
cp -R ../Pack world/datapacks
cp ../eula.txt ../server.properties ../io.txt .
gcc ../playio.c -o playio



