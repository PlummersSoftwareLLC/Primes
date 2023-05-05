# Batch solution by bt2585

![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-unknown-yellowgreen)

A Windows Batch file implementation of the prime sieve.

IMPORTANT NOTE: Because this implementation is file-based, disabling Anti-Virus, Windows Search, etc., will likely improve performance.

## Run instructions

Place the CMD on a disk/volume with at least 1GB of free space.

If you're going to use a VHD, you MUST have elevated privileges. DISKPART required admin privileges. Contiguous free space will be helpful. The VHD file is created next to the CMD.

`PrimeFiles NMAX=1000000 VHD=Y`

This will create a VHD, use the VHD for all its working files, then tear down and delete the VHD when done.

## Output
```
primeFiles.cmd nmax=1000000 vhd=y

:GETARGS
:DELETEVDISK
:CREATEVDISK
:FINDVHDDRIV
:CREATEFOLDERS
:DELETEFILES
:GETSQRT
:SIEVE
:COUNTPRIMES
:DELETEVDISK
PRIMECOUNT=78498
PrimeFiles;1;201.720;1;algorithm=base,faithful=no
```
This output was obtained from a sieve of 1,000,000 natively on Windows 10.

Machine specifications for completeness:
```
Intel Core i7-3770 CPU @ 3.40GHz
16 GB RAM
Samsung SSD 830 (250GB) on SATA2
```
This implementation uses all the tricks we could think of to squeeze every ounce of performance out of the code.

I tried using environment variables in various ways, but the shell just couldn't handle it, so I moved to disk, which was much faster without the certain environment limitations.

I tried creating folders, rather than files, but that proved difficult to work with.

We benchmarked binning the composites (~400,000 files) into folders, using the last 1, 2, or 3 digits of the composite number.
The thought was that Windows has trouble enumerating folders with larger numbers of files in them.
It turns out that NOT binning was fastest, because getting the last x digits of a number takes time.

We even benchmarked the various methods of creating empty files as markers.
BREAK>filename is the fastest we found.

We needed to create nearly 500,000 files (~80K primes, and ~400K for composites). That requires nearly 1GB of disk space and lot of small I/O.
The best place to store all these files is a nice pristine file system, like a freshly created and formatted VHD!

We benchmarked formatting the VHD as NTFS, exFAT, FAT32, and FAT.
- exFAT defaults to a 32KB cluster, but I formatted with a 4KB cluster. exFAT was incredibly slow.
- FAT16 defaults to a 32KB cluster, and required a ~5GB VHD.
- FAT32 defaults to a 4KB cluster, was faster than NTFS, but has a limit of 65536 files per folder.
- NTFS  defaults to a 4KB cluster, can handle large numbers of files in folders, and the VHD needs to be only 768MB, which takes just a couple seconds to create and format. NTFS wins

For fun, we created a 1GB RAMDisk to store the files. As expected, we got a performance bump, but we didn't think this was completely faithful to the challenge.
