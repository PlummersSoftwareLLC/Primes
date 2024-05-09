#!/bin/bash

if [ -d cbmbasic ]; then
    git -C cbmbasic pull
else
    git clone https://github.com/mist64/cbmbasic.git
fi

make -C cbmbasic