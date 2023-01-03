#!/bin/bash
if [ -n "$(docker images -q primes_algol68g 2>/dev/null)" ]
then
    docker rmi primes_algol68g:latest
fi
docker build . -t primes_algol68g:latest

