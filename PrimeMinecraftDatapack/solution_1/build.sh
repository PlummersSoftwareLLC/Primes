#!/bin/bash

# This script assumes that OpenJDK 17, wget and C build tools are already installed
# somewhere in $PATH.

mkdir env
cd env

wget https://launcher.mojang.com/v1/objects/a16d67e5807f57fc4e550299cf20226194497dc2/server.jar
cp ../eula.txt ../server.properties ../buildioscript.txt ../runioscript.txt .

gcc ../playio.c -o playio

./playio java -Xmx1024M -Xms1024M -jar server.jar nogui < buildioscript.txt

cp -R ../Pack world/datapacks



