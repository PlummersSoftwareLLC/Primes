#!/bin/bash

# This script assumes that OpenJDK 17.0.1 (or newer) is already installed
# somewhere in $PATH, and the build.sh script has been executed.

cd env
./playio java -Xmx1024M -Xms1024M -jar server.jar nogui < io.txt


