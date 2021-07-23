#!/bin/sh
docker build --pull --rm -f Dockerfile -t c:latest .
docker run --rm -it c:latest
