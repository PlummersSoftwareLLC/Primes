# APL solution by arcfide

![Algorithm](https://img.shields.io/badge/Algorithm-wheel-yellowgreen)
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Faithfulness](https://img.shields.io/badge/Faithful-yes-green)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

This is a basic implementation of a prime sieve using the algorithm initially conceived by Roger Hui and later refined by Jay Foad as documented here:

https://www.jsoftware.com/papers/50/

*Note: the build has been disabled at the request of the repository maintainers for the moment to avoid the inadvertent execution of the Dyalog APL interpreter, which is free for non-commercial use such as these demonstrations and benchmarks, but which is not Free Software.* 

## Run Instructions

You must have a working version of Dyalog APL installed. On Linux/UNIX, run `LOAD=PrimeSieveAPL.apln dyalog`. On Windows open the .dyapp file.

## Output

	arcfideDfn;6135;5.001;1;algorithm=wheel,faithful=no,bits=1
	arcfideDfnFaithful;6187;5.001;1;algorithm=wheel,faithful=yes,bits=1
	arcfideDfnBaseFaithful;2187;5.002;1;algorithm=base,faithful=yes,bits=1
	arcfideClassFaithful;1423;5.002;1;algorithm=wheel,faithful=yes,bits=1

## Terms and conditions for use of Dyalog

Dyalog is free for non-commercial use and for limited commercial use, but is not free software. You may create public docker images which include Dyalog APL in addition to your own work, if you observe the following conditions:

* You must include Dyalog's LICENSE file in a prominent location and include instructions which require it's inclusion in any derived works.

* If you do not have a commercial licence with a corresponding [Run Time Licence](https://www.dyalog.com/prices-and-licences.htm#runtimelic), and you make images available for download, the default Run Time Licence will automatically apply. This allows non-commercial and limited commercial distribution up to the revenue limit described set out in the [Licenses, Terms and Conditions](https://www.dyalog.com/prices-and-licences.htm).
