#!/bin/bash
SCRIPT="tools/$(basename "$0")"
if [ "$#" -lt 2 ]
then
    echo "usage: ${SCRIPT} <language> <number>"
    echo "where:"
    echo "- <language> corresponds to the 'Prime' directory for the specified language."
    echo "  This is case-insensitive."
    echo "- <number> is the solution number."
    echo ""
    echo "Examples:"
    echo ""
    echo "    ${SCRIPT} CPP 1"
    echo ""
    echo "or"
    echo ""
    echo "    ${SCRIPT} cpp 1"
    echo ""
    echo "Both of these correspond to the PrimeCPP/solution_1 directory."
    exit 1
fi

LANG_DIR="$(find . -mindepth 1 -maxdepth 1 -type d -iname "Prime$1" -exec basename '{}' ';' 2>/dev/null)"
if [ -z "${LANG_DIR}" ]
then
    echo "Cannot find corresponding 'Prime' directory for language $1"
    exit 1
fi

DOCKERFILE="${LANG_DIR}/solution_$2/Dockerfile"
if [ ! -f "${DOCKERFILE}" ]
then
    echo "'${DOCKERFILE}' does not exist"
    exit 1
fi

echo "Running hadolint on '${DOCKERFILE}'"
if docker run --rm -i -v "$(pwd)/config:/.config" hadolint/hadolint < "${DOCKERFILE}"
then
    echo "hadolint succeeded"
else
    echo "hadolint failed"
    exit 1
fi
