#!/usr/bin/env python3
import sys
from memdump_utils import read_memdump

memdump = read_memdump(sys.argv)

# Count primes
prime_count = 1
for val in memdump[0xbdc:]:
	prime_count += val.bit_count()

# Get timer read-out from status line
tstrb = memdump[0x0bd5:0x0bdc]

#b'00:14.2'
# -> ASCII and PETSCII overlap for these chars
timestr = tstrb.decode("ascii")

print("TIME:",timestr,sep="\t")
print("PRIMES:",prime_count,sep="\t")
print("VALID:",(prime_count==78498),sep="\t")