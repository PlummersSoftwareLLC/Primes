#! /usr/bin/scheme --program

; create the library, closest idiomatic analog of a class in scheme. You can,
; of course, create something like a class in scheme, but it is very
; unidiomatic. A completely separate library fully encapsulates all the state
; and logic like a class would.
(library
  (sieve (1))

  ; only the sieve and print-results functions are public
  (export sieve print-results)
  (import (chezscheme))

  ; historical results to compare against
  (define results-alist
    '((          10 . 4         )
      (         100 . 25        )
      (        1000 . 168       )
      (       10000 . 1229      )
      (      100000 . 9592      )
      (     1000000 . 78498     )
      (    10000000 . 664579    )
      (   100000000 . 5761455   )
      (  1000000000 . 50847534  )
      ( 10000000000 . 455052511 )))

  ; scheme doesn't have bit arrays, but it does have bytevectors between
  ; bytevectors and bitwise operations, it's not too tricky to implement bit
  ; arrays by hand
  (define (bit-array-get? array index)
    (let-values ([(byte bit) (div-and-mod index 8)])
      (bitwise-bit-set? (bytevector-u8-ref array byte) bit)))

  (define (bit-array-unset! array index)
    (let-values ([(byte bit) (div-and-mod index 8)])
      (bytevector-u8-set!
        array
        byte
        (logbit0 bit (bytevector-u8-ref array byte)))))

  ; actual sieve function
  (define (sieve n)
    (let ([a (make-bytevector (+ (div n 8) 1) 255)])
      (let outer-loop ([i 2])
        (if (<= (* i i) n)
          (if (bit-array-get? a i)
            (let inner-loop ([j (* i i)])
              (if (<= j n)
                (begin
                  (bit-array-unset! a j)
                  (inner-loop (+ j i)))
                (outer-loop (+ i 1))))
            (outer-loop (+ i 1)))
          a))))

  ; function to count the primes less than n, used for validation
  (define (count-primes n)
    (let ([sieve (sieve n)])
      (let loop ([i 3] [count 1])
        (if (< i n)
          (loop
            (+ i 2)
            (if (bit-array-get? sieve i) (+ count 1) count))
          count))))

  ; compare results from count-primes to historical results
  (define (validate-results n)
    (= (cdr (assq n results-alist)) (count-primes n)))

  ; convert scheme duration to fractional number of seconds
  (define (duration-to-seconds duration)
    (+ (time-second duration) (* 1e-9 (time-nanosecond duration))))

  ; print the results in the expected format
  (define (print-results duration passes n)
    (format #t "William103;~a;~a;1;algorithm=base,bits=1,faithful=yes" passes (duration-to-seconds duration))
    (newline)))
