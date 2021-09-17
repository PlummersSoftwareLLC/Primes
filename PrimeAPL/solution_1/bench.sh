#!/bin/sh

cat << EOF | dyalog -b -s 2> /dev/null | grep --color=never arcfide
)load salt
enableSALT
⎕SE.SALT.Load'./PrimeSieveAPL.apln'
PrimeSieveAPL.run⍬
⎕OFF
EOF
