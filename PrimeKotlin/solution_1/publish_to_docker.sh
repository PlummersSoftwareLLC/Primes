#!/bin/sh
# Author:   Frank van Bakel
# Date:     2020-07-21
# 
# Purpose:  This script build the base image that is used for the build.
#           The reason is that otherwise the build for Kotlin would take really 
#           long.
#
# Usage:    Only use this if there is a need to update the build image.
#
docker build --pull --rm -f CompileDockerfile -t fvbakel/kotlin-primes:1.0.0 .
docker push fvbakel/kotlin-primes:1.0.0
