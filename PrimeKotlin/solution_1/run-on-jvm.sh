#!/usr/bin/env bash

./gradlew fatJar
java -jar build/libs/PrimeKotlin-fat-1.0.0-SNAPSHOT.jar
