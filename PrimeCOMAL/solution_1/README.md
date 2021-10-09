# BITSIEVE.CML
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)
![Deviation](https://img.shields.io/badge/Deviation-sievesize-blue)

a prime sieve implementation in COMAL  
using a string as a pseudo-bit array for flags  
by @kottm, 2021-08-12  
inspired by Dave's Garage Prime Sieve  
written and compiled using Unicomal 3.11 (MS-DOS version)  

UniComal was one of the most widespread and influential versions of COMAL for MS-DOS and other platforms in the late 1980s.

To run it on modern computers, an emulator like DOSBox is required. A working version of UniComal 3.11 for MS-DOS, including a preconfigured DOSBox setup is available for the Internet Archive here:

<https://archive.org/details/uni-comal-3.11-16bitwindos-os-2>

Extracting the archive will create a full working version of UniComal3.11, including .BAT files for opening the COMAL environment directly in DOSBox. As COMAL was originally developed in Denmark, the help key (F1) is labelled with the Danish word "HJÆLP". The online documentation is, however, set for US English.

This is the system on which BITSIEVE.CML was developed, tested, and even complied to an MS-DOS .EXE file.
 
Sieve of Eratosthenes programs in various languages and systems were tested in *BYTE Magazine* for speed and size benchmarking already in vol. 6, no. 9 (1981), and vol. 8, no.1 (1983).

See:
- <https://archive.org/details/byte-magazine-1981-09>
- <https://archive.org/details/byte-magazine-1983-01>

These versions relied on an array of ints of bytes for the flags, instead of a bit array. Usually, normal arrays a 0-indexed, thus could set flags using this way of iterating:
    FOR i#:=0 TO size# DO
        IF flags#(i#) THEN
            prime#:=i#+i#+3
            FOR k#:=i#+prime# TO size# STEP prime# DO flags#(k#):=FALSE
            count#:+1
        ENDIF
    ENDFOR i#

which is also how it was done in the "SIEVE.CML" example included with UniComal (see the SUPP folder for the example files included with UniComal).

This implementation, which tries to more closely follow Dave's original concept, instead uses a character string as a kind of pseudo-bit array.

However, since this implementation uses a string, the bit positions in the string are indexed from 1, not 0, thus requiring a different way of calculating which flags to set. This results in the probably less tidy solution presented here.

Furthermore, being a product of the MS-DOS era with systems with many times less RAM than computers today, UniComal places limitations of 15,000 bytes for how large an array or string can be. This means that, using the pseudo-bit array solution here, the maximum number of primes for which this code can test is up to 100,000 (i.e. a byte string of 12,500 characters for flags).

COMAL systems usually saved programs in bytecode form using the .CML extension; for portability, however, programs could be dumped to plain text files using the LIST command (for output with line numbers) or DISPLAY (for output without line numbers). Using LIST is how the program file BITSIEVE.TXT uploaded here was created.

To load a text file as a program in COMAL, open the interpreter (in this case, open UniComal in DOSBox using the one of the provided .BAT files in the Comal directory), used the CHDIR "directory path" command to change to the director where you have downloaded bitsieve.txt, and use ENTER "BITSIEVE.TXT" to import the file as a program. Start the program with the RUN command.