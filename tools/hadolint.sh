#!/bin/bash
if [ "$#" -lt 2 ]
then
    echo "usage: tools/$(basename "$0") <language> <number>"
    echo "where:"
    echo "- <language> is language of your solution"
    echo "- <number> is the solution number"
    echo ""
    echo "Example:"
    echo ""
    echo "    tools/$(basename "$0") CPP 1"
    exit 1
fi

if [ ! -d "Prime$1/solution_$2" ]
then
    echo "Directory 'Primes$1/solution_$2' does not exist"
    exit 1
fi

echo "Running hadolint"
if docker run --rm -i -v "$(pwd)/config:/.config" hadolint/hadolint < "Prime$1/solution_$2/Dockerfile"
then
    echo "hadolint succeeded"
else
    echo "hadolint failed"
    exit 1
fi
