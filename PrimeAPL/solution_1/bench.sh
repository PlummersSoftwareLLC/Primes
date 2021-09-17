#!/bin/sh

cat << EOF | /opt/mdyalog/18.0/64/unicode/dyalog -b -s 2>/dev/null | grep --color=never arcfide
)load salt
enableSALT
⎕SE.SALT.Load'./PrimeSieveAPL.apln'
PrimeSieveAPL.run⍬
⎕OFF
EOF
