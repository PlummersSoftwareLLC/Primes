#!/bin/bash
python3 PrimePY.py "$@"
echo ""
python3 PrimePY_numba.py "$@"
