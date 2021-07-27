#! /usr/bin/scheme --script

; set maximum optimization level
(optimize-level 3)

; compile the library and program
(compile-library "sieve.ss")
(compile-program "PrimeScheme.ss")

; actually run the program
(load-program "PrimeScheme.so")

; cleanup
(delete-file "sieve.so")
(delete-file "PrimeScheme.so")
