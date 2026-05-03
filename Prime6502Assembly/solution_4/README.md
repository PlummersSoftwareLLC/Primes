# 6502 assembly solution by RazCamelot for Commodore 64
![Algorithm](https://img.shields.io/badge/Algorithm-base-green)
![Faithfulness](https://img.shields.io/badge/Faithful-no-yellowgreen)
![Parallelism](https://img.shields.io/badge/Parallel-no-green)
![Bit count](https://img.shields.io/badge/Bits-1-green)

**By:** Rasmus Wernersson, AKA Raz/CML  
April/May 2026 (original code mostly written in the summer of 2023)  
Blog-post, binaries download: [Extended resource page for PrimeSieve1m](https://wernersson.dk/c64/primesieve/primesieve.html)

**In overview:**  
- Entire set of primes up to 1,000,000 are found (using a bitfield of length 500,000 - 62500 (`$f424`) bytes).  
- Including the validation, it finishes in 13.8 sec (NTSC) / 14.2 sec (PAL) : (~ 13.35M cycles).   
- The main trick for speeding up the algorithm is to treat it like a bit filling exercise, where specific bit-patterns can quickly be rolled out over the entire bit-field. _It still holds true to the original algorithm_ - this is a type of inner-inner loop.  
- Main trick for having it fit in a C64 is treating it like a demo-effect or computer game: take over the entire machine as the first thing, and agressively utilize every bit of memory available (including having code run in ZP + some of the stack space etc).
- I've also included some nice visuals for tracking progress + stages of the algorithm. The color coding utilizes the trick that the 4-bit color ram fundamentally exists outside the ordinary ram). 
  
**Assembling the code**  
The code is written using KickAssembler. It does use some of the advanced features of KickAss, including:  
- Assembly of code with a virtual memory target. Mostly used for the code that ultimately gets copied to ZP (zero page), but which needs to be located elsewhere when the binary is loaded.  
- The KickAss scripting language for generating tables etc on the fly, rather that generating them outside (e.g. using Python) and importing them as `.byte` tables.  
- Segments. I make good use of KickAss' segment system to organize code and data, put in guard rails for size of sub-segments, and generally make life a lot easier for this slightly convoluted memory setup.

Assembly is simply: `java -jar KickAss.jar PrimeSieve1M.asm` which will produce the binary `PrimeSieve1M.prg`

KickAssembler: [https://theweb.dk/KickAssembler/](https://theweb.dk/KickAssembler/)

**Running the binary (interactively)**  
I recommend using VICE: [VICE - the Versatile Commodore Emulator](https://vice-emu.sourceforge.io/) - all testing and debugging done on VICE (with occational testing on real hardware, my setup being a C64C (PAL) with an Ultimate II+ cartridge for file transfer + 1541 emulation). 

**Timing the run (interactively)**   
The binary has a build in validation step, and it utilizes the C64 TOD (time of day) clock for the timing. The timer is auto calibrated for NTSC or PAL upon start. The start menu allows for running two variants of the the algorithm (with and with-out the lazy init optimization) as well as triggering the algorithm to miscount the primes for showing that the validation works.

![Start menu](https://wernersson.dk/c64/primesieve/figures/PrimeSieve1M_start_menu_CRT.jpg)

**Running and timing the binary in batchmode**

To help run and time the program automatically, it it can be build with ```"-define BATCHMODE"``` which produces a slightly different version of the binary that will autostart and enter an endless loop at 0x0a00 after completion. That can then be used as a hook for the Vice emulator to execute a dump of the memory contents for outside analysis.

Scripts:

```
autorun.sh
basevars.sh
build.sh
analyze_memdump.py
```
Edit ```basevars.sh``` to point to ```java``` and ```x64sc```.

Please see my blog-post for further pointers on how to play around with the memory dump: [Extended resource page for PrimeSieve1m](https://wernersson.dk/c64/primesieve/primesieve.html)

**Docker**

Finally, there is a Dockerfile for wrapping up the entire process and automating the build, running + checking of results (thanks to Rutger van Bergen for help with this).

## Results

```
RazCamelot-c64PAL;1;14.2;1;algorithm=base,faithful=no,bits=1
RazCamelot-c64NTSC;1;13.8;1;algorithm=base,faithful=no,bits=1
```

-----------------------------------------------------------

Implementation details
======================

I have done my best to seperate out what is C64 specfic and what is 6502/6510 general - the overall algorithm (with opitimizations) should be quite portable to other 6502 platforms.

Memory
------
**Bitfield** is placed at `$0bdc .. $ffff`. As we shall see in a moment, this allows for a nifty optimization regarding how we detect reaching the end of the bitfield as we fill in data.

Notice, that on the C64 this includes all memeory behind the ROM areas at `$a000-$bfff` and `$e000-$ffff` as well as the VIC + SID + CIA + Color ram areas in the span `$d000-$dfff`.  

Furthermore, the moment we shift out these areas by writing `#$30`to `$01` the system will expect the RESET/NMI/IRQ vectors at `$FFFA-$FFFF` to have sensible values in RAM rather than ROM. This is handled by clearing out all pending interrupts (IRQ + NMI), and completely disabling IRQs while the program is running, thus preventing the CPU to ever jump to the IRQ vectors. We'll handle updates to screen etc as part of the MAIN routine instead.

Finally, any updates we want to do to VIC and Color ram is handled by briefly mapping back in those areas (`#$35` -> `$01`) updating what we need and then swapping out the IO areas again.


Core algorithm
--------------

As we consider this particular implementation, it is worth starting out with the fundamentals. The Wikipedia page on the [Sieve of Erotasthenes](https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes) contains the following breif pseudocode as a way of compactly illustrating the algorithm:

```
algorithm Sieve of Eratosthenes is
    input: an integer n > 1.
    output: all prime numbers from 2 through n.

    let A be an array of Boolean values, indexed by integers 2 to n,
    initially all set to true.
    
    for i = 2, 3, 4, ..., not exceeding √n do
        if A[i] is true
            for j = i^2, i^2+i, i^2+2i, i^2+3i, ..., not exceeding n do
                set A[j] := false

    return all i such that A[i] is true.
```
- This implementation (as well as all other implementations in this challenge) utilize the trick of **only investigating the odd numbers** (and just taking for a fact, that we know 1 not to be prime and 2 to be prime).  
- We'll need to run through the numbers 3..sqroot(n) to fill in the full bitfield. Since our target n is 1.000.000 we'll evaluate 3..999 stepping 2 at the time in normal space and **1..499 in the odd-number space** represented in the bitfield (stepping 1 at the time). 
- When we have found a number to be prime, we'll need to mask out the number at position i^2 and every other position at +i distance after that. Since all even numbers are left out, **moving i steps in the bitfield corresponds to moving 2i steps in normal space** (we'll get back to this later).

**Bit patterns**:  
Let's look at an example of what happens when we have found 17 to be prime. The first number to fill in is 17^2 = 289 which would be bit 1 (289 AND 7) in byte 36 (289 >> 3) of the bitfield. Then we'll look at 289+17 = 306, whick is now bit 2 (306 AND 7), then it'll be 306+17 -> bit 3.

The bitpattern would be 1,2,3,4,5,6,7,0 and then back to 1. This can be generalized to the following:

```
Igonoring byte steps, listing bit steps for all odd numbers:

Number AND 7:
01,02,03,04,05,06,07,00 : 8 steps
03,06,01,04,07,02,05,00 : 8 steps
05,02,07,04,01,06,03,08 : 8 steps
07,06,05,04,03,02,01,00 : 8 steps

```
**Yes, that is obvious** - why is it interesting for anything? 
The point is that we always have 8 steps, and that the bit pattern for each step will repeat every "i" bytes (see below). That allows for a very efficient implementation on a platform without instructions for multiplication and multi-step bitshifts.
 
It will in the end be the same overall number of load/store operations, but we can eliminate some expensive per-step calculations. Furthermore, this approach lends itself well to a couple of other optimizations.

For each prime number "i" we need to mask out, we do the following:

- Utilize `((i^2) >> 3)` tables with bitfield address directly encoded
- Utilize tables for finding the starting bit to be updated: `(i^2) AND 7`
- Set up the adder of the fill routine with steps of `(i*8)>>3` (which is of course just "`i`")  bytes per step. 

Looping 8 times:

- Set starting address (byte level)
- Set step size (byte level)
- Load current mask to X (bit level)
- Call fill routine

The core of the fill routine is placed in ZP and looks like this:
 
 ```
.C:0015  A0 00       LDY #$00
.C:0017  18          CLC
.C:0018  B1 1B       LDA ($1B),Y
.C:001a  8F E3 0B    SAX $0BE3
.C:001d  A5 1B       LDA $1B
.C:001f  69 0B       ADC #$0B
.C:0021  85 1B       STA $1B
.C:0023  90 F3       BCC $0018
.C:0025  A5 1C       LDA $1C
.C:0027  69 00       ADC #$00
.C:0029  85 1C       STA $1C
.C:002b  90 EB       BCC $0018
 ```
(_This is the state of the code just as we enter the main loop for filling out the bitfield for the prime "11"_)
 
**Notice:**

- Address `$001b` doubles as a normal ZP pointer to use in the `LDA($1B),Y` instruction as well as the direct address in the `SAX $XXXX` instruction.  
- `SAX`is a stable undocumented opcode (works on all 6510 variants as well as all non-65c02 variants of the MOS 6502). It effectively stores the result of (A AND X) allowing us to **use X as an AND mask** for all the operations here. 
- Furthermore, Y is kept at `#$00` meaning the LDA(ZP),Y operation never overflows a page, and therefore the load+mask+store operation **only takes 9 cycles** in total (5 LDA, 4 SAX). 
- We also only need to update a single set of address pointers. In most cases only the low-byte needs to be updated.
- Finally in the cases where the high-byte of the pointer is updated, we can simply check for overflow of the bitfield by checking carry, since the bitfield ends at `$ffff`.
- The BCC at `$0023` is  replaced with NOP#IMM when we get to the point where the ADC at `0027` is adding a non-zero value. 

Complexity analysis
-------------------

In the core aligorithm the inner loop is run **811.068** times. Intuitively, we expect that the smaller primes will be responsible for a lot of this, as they trigger the need to flip a lot of bits in the bitfield.

In details the breakdown looks like this:

```
field  norm    loops   %_total  runningT  %_runtot
    1     3   166666   20.5490    166666   20.5490
    2     5    99998   12.3292    266664   32.8781
    3     7    71426    8.8064    338090   41.6845
    5    11    45450    5.6037    383540   47.2883
    6    13    38456    4.7414    421996   52.0297
    8    17    29404    3.6253    451400   55.6550
    9    19    26307    3.2435    477707   58.8985
   11    23    21728    2.6789    499435   61.5775
   14    29    17227    2.1240    516662   63.7014
   15    31    16114    1.9868    532776   65.6882
   18    37    13496    1.6640    546272   67.3522
   20    41    12175    1.5011    558447   68.8533
   21    43    11607    1.4311    570054   70.2844
   23    47    10615    1.3088    580669   71.5931
   26    53     9408    1.1600    590077   72.7531
   29    59     8446    1.0413    598523   73.7944

  125   251     1867    0.2302    738809   91.0909
  128   257     1818    0.2241    740627   91.3150

  473   947       55    0.0068    810899   99.9792
  476   953       49    0.0060    810948   99.9852
  483   967       34    0.0042    810982   99.9894
  485   971       30    0.0037    811012   99.9931
  488   977       24    0.0030    811036   99.9961
  491   983       18    0.0022    811054   99.9983
  495   991       10    0.0012    811064   99.9995
  498   997        4    0.0005    811068  100.0000

```
(Notice that: Field = bf offset/odd-space counter, norm = value in normal space). 

Filling in the bitfield for the prime "3" alone accounts for 20.5 percent of the inner loop time, and the first 5 primes (3,5,7,11,13) accounts for 52 percent of the inner loop time.
Furthermore, for the first few primes (3,5,7) we have a total of **338.090** updates to a bitfield of length **62.500** bytes, which is very costly on a platform with zero mem caching (_much more about this in the section on **Lazy bitfield init**_)).

Also, the optimization in the inner loop for handling updates of the low-byte of the address pointer as efficiently as possible, at a slight cost (2 cycle) in situations where the high-byte needs updating is indeed a big time saver. Doing the full 16-bit addition (`LDA $1b, ADC #XX, STA $1b, LDA $1c, ADC #YY, STA $1c`) before an anyhow needed `BCC` would cost 16 cycle instead of the 8 we're currently spending. The point where the early BCC at address `$0023` becomes irelevant is when the `ADC #xx` at adress `$001f` approaches `#$ff` at which point we have already completed around 90 percent of the computation. Having a second inner loop optimized for the "high primes" would save appx. 150.000 cycles (ca. 0.15 seconds) but would come at the cost of the need for a bit of extra space + control code.

Bitfield representation
-----------------------

Small aside, before we go into the pattern optimizations. I have opted for an internal representation of the bits in the bitfield that is **layed-out the same way as C64 bitmap graphics**. 

If bits 0,3,6,7 are set the byte will look like this:  
```
10010011
```

Whereas it would be reversed if we're just counting bits from the beginning:  
```
11001001
```

Having the bits organized as graphics makes it very easy to inspect the bit patterns via the Vice machine code monitor - e.g.:

```
(C:$0edc) mc 0bdc
>C:0bdc .###.##. 76
>C:0bdd ##.#..## d3
>C:0bde ..#.##.# 2d
>C:0bdf ..#..##. 26
>C:0be0 .#.##..# 59
>C:0be1 .#..#... 48
>C:0be2 #.##.##. b6
>C:0be3 #......# 81
```
I also had an original plan of displaying the bitfield as graphics, which I did not get around to implement.  
**Important:** Organizing the bits this way have ZERO impact on performance and output of the algorithm - it's purly aesthetics.

Lazy init of bitfield
---------------------

The main way to shave off a significant amount of time, is to come up with a more effecient way to handle those "expensive early primes". The insight here is to look for **repeating byte patterns**. From our ealier analysis of **bit patterns** we know that the pattern repeates every "i" bytes, where "i" is the value of the prime we're working with (in normal space). Looking at the first few primes these are the relevant observations:

```
Normal space   Odd space
*------------> *----------->
norm    n^2    field  fillBy  fillBi  ByRep
   3      9        1       4       1      3
   5     25        2      12       2      5
   7     49        3      24       3      7
  11    121        5      60       5     11  
```
Sidestepping some uniqueness at the start of the bitfield for now, we can study what the repeating patterns looks like:

```
Repeating pattern for "3":
1011.0110 1101.1011 0110.1101 | 1011.0110 ...

Repeating pattern for "5":
1101.1110 1111.0111 1011.1101 1110.1111 0111.1011 | 1101.1110 ...

Repeating pattern for "7":
1110.1111 1101.1111 1011.1111 0111.1110 1111.1101 1111.1011 1111.0111 | 1110.1111 ...

```
If we AND the patterns together, we'll need 15 bytes (`3*5`) to loop back to the beginiing of the pattern for combining 3+5, and we'll need 105 bytes (`3*5*7`) for combining 3+5+7 and so forth.

**The "Lazy init" idea**:

Initilialize only the 1st byte of the field to `#$ff` and set the (byte) length to "1".
As we iterate through the numbers do the following if a prime "i" is found:  
1) Extend length to prev_len * i.  
2) Fill out the new buffer with the contents from the previous (repeat the byte block "i" times in total).  
3) Fill in the bit patterns for prime "i" as in the standard algorithm.  
-> once we have filled in the entire buffer of 62500 bytes, we need to fix the beginning of the field to reflect that we did find 3,5,7 etc to be prime, and then the original algorithm can pick up.

This would give the following progression:

```
field  norm  loops  bytes_cp  new_len  loops_saved
    0     1      -         -        1            -
    1     3      8         2        3       166658 
    2     5     24        12       15        99974
    3     7    120        90      105        71306
    4     9      -         -      105            -
    5    11    840      1050     1155        44610
    6    13   9240     13860    15015        29216
    7    15      -         -    15015            -
    8    17  29404*    47485*   62500*  (Overflow, full buffer filled)
```

Total saved loops: 411.764, loops still needed: 811068-411764 = 399.304

**Conclusion:** This should **cut the inner loop runnng time roughly in half**, and this should be applicable to any implementation in any language. It does come at a cost of a more complicated intialization procedure.

**The actual "Lazy init" implementation**:

As we're already pushing the envelope squeezing in code in our limited space, the lazy init has been implemented as a compromise gaining a fair bit of the speed-up without modifying the code too much.

Concretely:  
1. The lazy init buffer length is set to 105 bytes (`1*3*5*7`) and is placed at the very end of memeory (allowing use of an unmodified inner loop).  
2. A special version of the i^2 tables are used to inject the modified start adress as well as the behaviour to fill out the _entire_ buffer with the bit-patterns.  
3. Bitfield original start at `$0bdc` is set to `#%01111111` as normal - the core algorithm does not notice it is hacked.  
4. Core algorithm is run for indices 0-3 (incl) evaluating 1,3,5 and 7 as normal.  
5. Buffer is folded unto to entire bitfield, and starting byte is set to combine results from indices 0-3 and bitfield roll-out, we're thus NOT "cheating" in any way by hardcoding that we know 3,5 and 7 to be prime.

**Compromise lazy init complexity:**

```
field  norm  loops  bytes_cp  new_len  loops_saved
    0     1      -         -      105            -
    1     3    280         -      105       166386 
    2     5    168         -      105        99830
    3     7    120     62395      105        71306
```
Total saved loops: 337.522, loops still needed: 811068-337522 = 473.546

**In summary:** we cut down the inner loop time to around `58.3%` of the original time via this compromised lazy init implementation. 

-----------------------------------------------------------

Visualization
-------------

As virtually all of the C64 memory is in use, progress is visualized as color coding untop of some of the data tables (the 4-bit color ram in essence exists outside the main 64kb memory).  
- green char at the top left is frequently updated to indicate progress.  
- factor are colored in white as they are found.  
- non-factors are blacked out as they are encountered.  
- yellow = part of the set 1..499 not investigated yet.  

![Start menu](https://wernersson.dk/c64/primesieve/figures/PrimeSieve1M_color_coding_example.jpg)

Once all factors have been found, the validation step starts. It runs through all bytes in the bitfield and count up the number of primes found (it uses a 256 byte look-up table to quickly count the number of bits set in each byte, and a 24 bit counter).
Upon validation the timer will stop, and the screen look like this: 

![Validation complete](https://wernersson.dk/c64/primesieve/figures/PrimeSieve1M_validation_complete.jpg)

(You can at this point press space to run the algorithm again, or drop into the Vice machine code monitor to have a look at the bitfield directly).



