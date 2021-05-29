#!/usr/bin/env bash

function run_bat()
{
    "$currentDir/gradlew.bat" build
}

# honestly i just like that name
function run_dat()
{
    "$currentDir/gradlew" build
}

currentDir="`dirname $(readlink -f "$0")`"
cd "$currentDir"

if [ "$(uname)" == "Darwin" ]; then
    run_dat     
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
    run_dat
elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW32_NT" ]; then
    run_bat
elif [ "$(expr substr $(uname -s) 1 10)" == "MINGW64_NT" ]; then
    run_bat
fi

echo "Build completed, executing prime sieve"
java -jar "$currentDir/build/libs/PrimeSieveJava.jar"

exit 0
