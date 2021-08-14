#!/bin/sh
echo 1000000 | ./dacvs1 | awk '
    1 { print > "/dev/stderr" }
    $1=="I" {i=$2}
    $1=="T" {t=$2}
    END {
        printf("%.1f\n", i*1000000000/t) > "/dev/stderr"
        printf("dacvs1;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", i, t/1000000000)
    }
'
