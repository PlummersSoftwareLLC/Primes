#!/bin/sh

nasm -felf64 cwager_x64ff_mt.asm -o cwager_x64ff_mt.o
gcc -no-pie -pthread cwager_x64ff_mt.o -o cwager_x64ff_mt
