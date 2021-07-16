#!/bin/bash

cd "${0%/*}"

exec find . -name '*.sh' -and -not -name run.sh -exec {} \;
