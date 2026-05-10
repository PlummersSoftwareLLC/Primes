#!/bin/sh

nasm -felf64 cwager_x64ff_mt.asm -o cwager_x64ff_mt.o
gcc -no-pie -pthread cwager_x64ff_mt.o -o cwager_x64ff_mt

nasm -felf64 cwager_x64ff_mt_extreme.asm -o cwager_x64ff_mt_extreme.o
gcc -no-pie -pthread cwager_x64ff_mt_extreme.o -o cwager_x64ff_mt_extreme

nasm -felf64 cwager_x64ff_mt_extreme_maskgen_onefactor.asm -o cwager_x64ff_mt_extreme_maskgen_onefactor.o
gcc -no-pie -pthread cwager_x64ff_mt_extreme_maskgen_onefactor.o -o cwager_x64ff_mt_extreme_maskgen_onefactor
