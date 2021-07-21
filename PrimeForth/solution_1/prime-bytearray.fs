
: store-multiple ( [...] addr count -- )
      0 u+do dup i cells + rot swap ! loop drop ;

variable valid-prime-counts ( -- addr )
\ Historical data for validating our results - the number of primes
\ to be found under some limit, such as 168 primes under 1000
          4 10
         25 100
        168 1000
       1229 10000
       9592 100000
      78498 1000000
     664579 10000000
    5761455 100000000
valid-prime-counts 16 dup cells allot store-multiple

\ get the number of primes expected for a given sieve size, or -1 if unknown
: get-valid-prime-count ( sieve-size -- )
      dup
      8 0 u+do
          valid-prime-counts i 2* cells + @ over = if
                valid-prime-counts i 2* 1+ cells + @
                nip
                leave
            endif
          loop
      tuck = if -1 nip endif ;
     

: new-bitarray ( len -- addr )
      here swap
      allot
      ;

: dealloc-bitarray ( addr len -- )
      dup rot + here = if
            negate allot
        else
            drop \ can't deallot if we're not at the top of the stack
        endif
        ;

: get-bit ( addr idx -- bit )
      + c@ ;

: set-bit ( addr idx -- )
      + 1 swap c! ;

: clear-bit ( addr idx -- )
      + 0 swap c! ;

: set-all-bits ( addr len -- )
      0 u+do
            dup i + 1 swap c!
        loop drop ;

: clear-all-bits ( addr len -- )
      0 u+do
            dup i + 0 swap c!
        loop drop ;

: new-primes-bitarray ( sieve-size -- addr )
        1+ 2/ dup new-bitarray
        dup rot set-all-bits ;

: del-primes-bitarray ( addr sieve-size -- )
        1+ 2/ dealloc-bitarray ;

: get-prime-bit ( addr prime -- bit )
        dup 1 and if \ odd number
            2/ get-bit
        else
            2drop 0
        endif ;

: clear-prime-bit ( addr prime -- )
        2/ clear-bit ;

: run-sieve { addr sieve-size -- }
        sieve-size s>f fsqrt f>s    \ q
        3                           \ factor
        begin 2dup >= while         \ >= rather than > because we're comparing integers
            \ find next prime
            sieve-size 1- over u+do
                addr i get-prime-bit if 
                    i nip \ replace factor with loop counter
                    leave
                endif 
            loop

            \ mark off multiples
            dup 3 *
            begin dup sieve-size < while
                addr over clear-prime-bit
                over 2* +
            repeat
            drop \ drop the loop counter

            2 + 
        repeat
        2drop ;

: print-all-primes ( addr sieve-size -- )
        2 . cr
        3 u+do
            dup i get-prime-bit if
                i . cr
            endif
        loop
        drop ;

: count-primes ( addr sieve-size -- )
        1 swap
        2/ 1- 1 u+do
            over i 2* 1+ get-prime-bit if
                1+
            endif 
        loop
        nip ;

: validate-primes-result ( addr sieve-size -- )
        tuck count-primes
        swap get-valid-prime-count
        = ;

: benchmark-prime-sieve ( timeout sieve-size -- felapsed iterations )
        swap s>f 1e6 f*
        \ set up counter
        0 swap
        \ get start time
        utime d>f
        fswap
        begin
            fover utime d>f fswap f- fover f< while
                dup new-primes-bitarray over
                2dup run-sieve
                del-primes-bitarray
                swap 1+ swap
            repeat
        fdrop utime d>f fswap f-
        drop
        ;
        

: print-benchmark-results ( timeout sieve-size -- )
        benchmark-prime-sieve
        1e6 f/
        ." tjol-8bit;" . ." ;" f. ." ;1;algorithm=base,faithful=no,bits=8" cr
        ;

: print-primes ( sieve-size -- )
        dup new-primes-bitarray over
        2dup run-sieve
        2dup print-all-primes
        del-primes-bitarray
        ;

: run-validation ( sieve-size -- )
        dup new-primes-bitarray over
        2dup run-sieve
        2dup validate-primes-result
        if ." OK" cr else ." INVALID" endif
        del-primes-bitarray
        ;

