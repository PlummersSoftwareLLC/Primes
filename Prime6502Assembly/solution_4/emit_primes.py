#!/usr/bin/env python3
import sys
from memdump_utils import read_memdump

memdump = read_memdump(sys.argv)
bitfield = memdump[0xbdc:]

l_primes = ["2"]
for i in range(0,len(bitfield)):
	val = bitfield[i] # Implicit cast to int
	for j in range(0,8):
		if (val & (1<<(7-j))):   # Bit order: as C64 graphics
			oddspace = (i*8) + j
			normspace = (oddspace*2) + 1
			l_primes.append(str(normspace))

print("\n".join(l_primes))