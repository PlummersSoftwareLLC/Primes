#!/bin/sh
echo 1000000 | ./dacvs8 | awk '
    1 { print > "/dev/stderr" }
    $1=="I" {i=$2}
    $1=="T" {t=$2}
    END {
        printf("%.1f\n", i*1000000000/t) > "/dev/stderr"
        printf("dacvs8;%d;%f;1;algorithm=base,faithful=yes,bits=8\n", i, t/1000000000)
    }
'
