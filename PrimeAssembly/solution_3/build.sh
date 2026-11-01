#!/bin/bash

# This script assumes that OpenJDK, wget, git and gcc are already installed
# somewhere in $PATH.

EMUSTUDIO_ARCHIVE=emuStudio-0.41.tar

wget https://github.com/emustudio/emuStudio/releases/download/RELEASE-0.41/${EMUSTUDIO_ARCHIVE}
tar xvf ${EMUSTUDIO_ARCHIVE}

git clone --branch v1.1.4 --depth 1 https://github.com/rbergen/PlayIO.git
gcc PlayIO/playio.c -o playio