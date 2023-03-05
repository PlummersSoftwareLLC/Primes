#!/usr/bin/env bash
set -e
docker build --pull --rm -f Dockerfile -t fvbakel/kotlin-run:latest .
docker run --rm -it fvbakel/kotlin-run:latest