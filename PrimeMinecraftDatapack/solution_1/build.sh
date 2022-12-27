#!/bin/bash

# This script assumes that OpenJDK 17, wget and C build tools are already installed
# somewhere in $PATH.

mkdir env
cd env

wget https://piston-data.mojang.com/v1/objects/c9df48efed58511cdd0213c56b9013a7b5c9ac1f/server.jar
cp ../eula.txt ../server.properties ../buildioscript.txt ../runioscript.txt .

gcc ../playio.c -o playio

./playio java -Xmx1024M -Xms1024M -jar server.jar nogui < buildioscript.txt

cp -R ../Pack world/datapacks



