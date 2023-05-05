#! /usr/bin/env bash
echo "$BASH_VERSION"

cd "${0%/*}"

exec find . -name '*.sh' -and -not -name run.sh -exec {} \;
