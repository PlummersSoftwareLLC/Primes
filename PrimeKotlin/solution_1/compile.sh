#!/bin/sh
# gradle clean 
gradle jvmMainClasses shadowJar
gradle jsNodeProductionRun
gradle linkReleaseExecutableNative