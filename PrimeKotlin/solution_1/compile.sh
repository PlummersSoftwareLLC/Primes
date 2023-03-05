#!/usr/bin/env bash
# gradle clean 
gradle jvmMainClasses shadowJar
gradle jsNodeProductionRun
# Building the Kotlin/Native executable on Linux ARM64 will fail;
# Kotlin/Native does not support Linux for ARM64 platforms.
if [[ "$(arch)" =~ "^arm*" ]]; then
	gradle linkReleaseExecutableNative
fi