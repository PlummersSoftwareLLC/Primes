#!/bin/sh
echo 1000000 | ./dacvs1 | awk '
    $1=="I" {i=$2}
    $1=="T" {t=$2}
    END {
        printf("dacvs1;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", i, t/1000000000)
    }
'
echo 1000000 | ./dacvs8 | awk '
    $1=="I" {i=$2}
    $1=="T" {t=$2}
    END {
        printf("dacvs8;%d;%f;1;algorithm=base,faithful=yes,bits=8\n", i, t/1000000000)
    }
'
