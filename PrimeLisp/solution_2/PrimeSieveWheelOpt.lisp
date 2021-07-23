;;;; Common Lisp port of PrimeC/solution_2/sieve_5760of30030_only_write_read_bits.c by Daniel Spangberg
;;;
;;; run as:
;;;     sbcl --script PrimeSieveWheelOpt.lisp
;;;
;;; For Common Lisp bit ops see https://lispcookbook.github.io/cl-cookbook/numbers.html#bit-wise-operation,
;;; although most of the shifts were replaced by "normal" Lisp functions,
;;; e.g. x>>n -> (floor x m), a&b -> (mod a b), 1<<x -> (expt 2 x),
;;; because sbcl optimizes these rather efficiently.
;;; Only logior remains.
;;;


(declaim
  (optimize (speed 3) (safety 0) (debug 0))

  (inline nth-bit-set-p)
  (inline set-nth-bit))


(defparameter *list-to* 100
  "list primes up to that number, set to nil to disable listing")


(defconstant +results+
  '((         10 . 4        )
    (        100 . 25       )
    (       1000 . 168      )
    (      10000 . 1229     )
    (     100000 . 9592     )
    (    1000000 . 78498    )
    (   10000000 . 664579   )
    (  100000000 . 5761455  )
    ( 1000000000 . 50847534 )
    (10000000000 . 455052511))
  "Historical data for validating our results - the number of primes
   to be found under some limit, such as 168 primes under 1000")


; Should match machine register size for efficient code.
; Too small hurts a litle, too big hurts a lot
; because sbcl will be forced to use bignums
#+64-bit (defconstant +bits-per-word+ 64)
#-64-bit (defconstant +bits-per-word+ 32)

; Apparently some Lisps have non-negative-fixnum, sbcl doesn't.
; Also it's not in the ANSI CL specs.
(deftype nonneg-fixnum ()
  `(integer 0 ,most-positive-fixnum))

(deftype sieve-bitpos-type ()
  `(integer 0 ,(1- +bits-per-word+)))

(deftype sieve-element-type ()
  `(unsigned-byte ,+bits-per-word+))

(deftype sieve-array-type ()
  `(simple-array sieve-element-type 1))


(defconstant +steps+ #(
 8 1 2 3 1 3 2 1 2 3 3 1 3 2 1 3 2 3 4 2 1 2 1 2 7 
 2 3 1 5 1 3 3 2 3 3 1 5 1 2 1 6 6 2 1 2 3 1 5 3 3 
 3 1 3 2 1 3 2 7 2 1 2 3 4 3 5 1 2 3 1 3 3 3 2 3 1 
 3 2 4 5 1 5 1 2 1 2 3 4 2 1 2 6 4 2 1 3 2 3 6 1 2 
 1 6 3 2 3 3 3 1 3 5 1 2 3 1 3 3 2 1 5 1 5 1 2 3 3 
 1 3 3 2 3 4 3 2 1 3 2 3 4 2 1 3 2 4 3 2 4 2 3 4 5 
 1 5 1 3 2 1 2 1 5 1 5 1 2 1 2 7 2 1 2 3 3 1 3 2 4 
 5 4 2 1 2 3 4 3 2 3 3 3 1 3 3 2 1 2 3 1 5 1 2 1 5 
 1 5 1 3 2 4 3 2 1 2 3 3 4 2 1 3 5 4 2 1 3 2 4 5 3 
 1 2 4 3 3 2 1 2 3 1 3 2 3 1 5 6 1 2 1 2 3 1 3 2 1 
 2 6 1 3 3 5 3 4 2 1 2 1 2 4 3 6 2 3 1 6 2 1 2 3 4 
 2 1 2 1 6 5 1 2 1 2 3 1 5 1 2 3 4 3 2 1 3 2 3 4 2 
 3 1 2 4 3 2 3 1 2 3 1 3 3 2 3 3 4 3 2 1 5 1 5 1 2 
 1 5 1 3 2 1 5 3 1 3 2 1 3 2 3 4 3 2 1 6 5 3 1 2 3 
 1 6 2 1 2 4 3 2 1 2 1 5 1 5 3 1 2 3 1 3 2 1 5 3 1 
 3 2 6 3 4 3 2 1 2 4 3 2 3 1 2 3 4 3 3 2 3 1 3 2 1 
 2 1 5 6 1 2 6 1 3 2 1 2 3 3 1 6 3 2 9 1 2 1 2 4 3 
 2 3 1 2 4 3 3 2 1 2 3 1 3 2 1 2 6 1 6 3 2 3 1 3 2 
 3 3 3 1 3 2 1 3 2 3 4 2 1 2 1 2 7 2 3 1 5 1 3 3 2 
 1 5 1 5 1 2 7 5 1 2 1 2 3 1 3 5 3 3 1 5 1 3 2 3 4 
 2 1 2 3 4 3 5 1 2 3 1 3 3 2 1 2 3 1 3 2 1 3 5 1 5 
 3 1 2 3 4 2 1 2 6 1 3 2 1 3 2 3 6 1 2 1 2 4 3 2 3 
 1 5 1 3 5 3 3 1 3 2 1 2 1 5 1 6 2 3 3 1 6 2 3 3 1 
 3 2 1 3 2 7 2 1 3 2 4 3 2 3 1 2 3 4 3 3 5 1 3 2 3 
 1 5 1 5 1 2 1 2 4 3 2 1 2 3 3 4 2 4 2 3 4 2 1 2 1 
 6 3 2 3 3 3 1 3 3 2 1 2 3 1 3 3 2 1 5 1 5 1 3 2 3 
 1 3 2 1 2 3 7 2 1 3 5 4 2 1 2 1 2 4 5 4 2 4 3 5 1 
 2 3 1 3 2 3 1 5 1 5 1 2 1 2 3 4 2 1 2 3 3 1 3 3 3 
 5 4 2 1 2 3 4 3 2 4 2 3 1 3 3 2 1 2 3 6 1 2 1 5 1 
 5 1 2 1 2 4 5 1 2 3 4 3 2 1 3 2 3 4 2 4 2 4 3 2 3 
 1 2 3 1 3 3 2 3 3 1 3 3 2 1 5 6 1 2 1 2 3 1 3 2 1 
 8 1 3 2 1 5 3 4 2 1 2 1 6 3 5 1 2 3 1 6 2 1 2 4 3 
 2 1 2 1 6 5 3 1 2 3 1 3 2 1 2 3 3 1 3 2 1 5 3 4 5 
 1 2 4 3 2 3 1 2 3 1 3 3 3 2 3 4 2 1 2 1 5 6 1 2 1 
 5 1 3 2 1 2 3 3 1 5 1 3 2 7 3 2 1 2 4 5 3 1 2 3 1 
 3 3 2 1 2 4 3 2 1 2 6 1 6 2 1 2 3 1 3 2 1 5 3 1 3 
 2 4 2 3 4 2 1 2 1 2 7 2 3 1 5 4 3 2 1 2 3 1 5 1 2 
 1 6 5 1 2 3 3 1 3 2 3 3 3 1 3 3 3 2 3 6 1 2 3 4 3 
 5 1 2 4 3 3 2 1 2 3 1 3 2 1 3 5 1 5 1 3 2 3 4 2 3 
 6 1 3 2 1 3 2 3 6 1 2 1 2 7 2 3 1 2 3 1 3 5 1 5 1 
 3 2 1 2 6 1 5 1 2 3 3 1 3 3 2 3 3 1 5 1 3 2 3 4 2 
 1 3 2 4 3 2 3 1 2 3 4 3 2 1 5 1 3 2 1 3 5 1 5 3 1 
 2 4 3 2 1 2 3 3 1 3 2 4 2 3 4 2 1 2 1 2 4 3 2 3 6 
 1 3 3 2 3 3 1 3 2 1 2 1 5 1 6 3 2 3 1 5 1 2 3 3 4 
 2 1 3 9 2 1 2 1 2 4 5 3 1 2 4 3 3 3 2 3 1 3 2 3 1 
 5 1 5 1 2 1 2 3 1 3 2 1 2 3 3 4 3 3 2 3 4 2 1 2 1 
 6 3 2 6 3 1 3 3 2 1 2 3 4 3 2 1 5 1 5 1 2 1 2 3 1 
 5 1 2 3 4 3 2 1 3 2 3 4 2 3 1 2 4 3 2 4 2 3 1 3 5 
 3 3 1 3 3 2 1 5 1 5 1 2 1 2 3 4 2 1 5 3 1 3 2 1 3 
 5 4 2 1 2 7 3 2 3 1 2 3 1 6 2 1 2 4 5 1 2 1 5 1 5 
 3 1 2 4 3 2 1 2 3 3 1 3 2 1 5 3 4 3 3 2 4 3 2 3 1 
 2 3 1 3 3 3 2 3 1 3 2 1 2 1 5 6 1 2 1 5 1 3 2 1 2 
 6 1 5 1 5 7 2 1 2 1 2 4 3 5 1 2 3 1 6 2 1 2 3 1 3 
 2 1 2 7 6 2 1 2 3 1 3 2 1 2 3 3 1 3 2 1 3 2 3 4 2 
 3 1 2 7 2 3 1 5 1 3 3 2 1 2 3 6 1 2 1 6 5 1 2 1 5 
 1 3 2 3 3 3 1 3 2 1 3 2 3 4 3 2 3 4 8 1 2 3 1 3 3 
 2 1 2 4 3 2 1 3 5 1 5 1 2 1 2 3 4 2 1 8 1 3 2 4 2 
 3 6 1 2 1 2 4 3 2 3 1 2 3 4 5 1 2 3 1 3 2 1 2 1 5 
 1 5 1 2 3 3 1 3 3 2 3 3 1 3 3 3 2 3 6 1 3 2 4 3 2 
 3 1 2 7 3 2 1 5 1 3 2 1 2 1 5 1 5 1 3 2 4 3 2 3 3 
 3 1 3 2 4 2 3 4 2 1 2 1 2 7 2 3 3 3 1 3 3 2 1 5 1 
 3 2 1 2 6 1 5 1 3 2 3 1 3 3 2 3 3 6 1 3 5 4 2 1 2 
 1 2 4 5 3 1 2 4 3 3 2 1 2 3 1 3 2 4 5 1 5 3 1 2 3 
 1 3 2 1 2 3 3 1 3 3 3 2 3 4 2 1 2 1 2 4 3 2 4 5 1 
 3 3 2 3 3 4 2 1 2 1 5 1 6 2 1 2 3 1 5 1 2 3 4 3 2 
 1 3 2 7 2 3 1 2 4 3 2 3 1 2 3 1 3 3 5 3 1 3 5 1 5 
 1 5 1 2 1 2 3 1 3 2 1 5 3 4 2 1 3 2 3 4 2 1 2 1 6 
 3 2 3 3 3 1 6 2 1 2 4 3 3 2 1 5 1 5 3 1 2 3 1 3 2 
 1 2 3 4 3 2 1 5 3 4 3 2 1 2 4 3 2 4 2 3 1 3 6 2 3 
 1 3 2 1 2 1 5 6 1 2 1 5 4 2 1 2 3 3 1 5 1 3 9 2 1 
 2 3 4 3 2 3 1 2 3 1 3 3 2 1 2 3 1 5 1 2 6 1 6 2 1 
 2 4 3 2 1 2 3 3 1 3 2 1 3 2 3 4 2 1 3 2 7 2 3 1 5 
 1 3 3 2 1 2 3 1 5 1 2 1 11 1 2 1 2 3 1 3 2 3 6 1 3 
 2 1 5 3 4 2 1 2 3 4 3 5 1 2 3 1 6 2 1 2 3 1 3 2 1 
 3 6 5 1 2 1 2 3 4 2 1 2 6 1 3 2 1 3 2 3 6 3 1 2 4 
 3 2 3 1 2 3 1 3 5 1 2 3 4 2 1 2 1 5 1 5 1 2 6 1 3 
 3 2 3 3 1 3 2 1 3 2 3 4 3 3 2 4 5 3 1 2 3 4 3 2 1 
 6 3 2 1 2 1 5 1 5 1 2 1 2 4 3 2 1 5 3 1 3 2 4 2 3 
 4 2 1 2 1 2 4 3 2 3 3 3 4 3 2 1 2 3 1 3 2 1 2 1 5 
 1 5 1 5 3 1 3 2 1 2 3 3 4 3 3 5 6 1 2 1 2 4 5 3 1 
 2 4 3 3 2 1 2 3 1 3 2 3 1 5 1 5 1 3 2 3 1 3 2 3 3 
 3 1 3 3 3 2 3 4 2 1 2 1 2 7 2 4 2 3 1 3 3 2 1 5 4 
 2 1 2 6 1 5 1 2 1 2 3 1 6 2 3 4 5 1 3 2 3 4 2 3 1 
 2 4 3 2 3 1 2 3 1 3 3 2 3 3 1 3 3 3 5 1 5 3 1 2 3 
 1 3 2 1 5 3 1 3 2 1 3 2 3 4 2 1 2 1 6 3 2 3 1 5 1 
 6 2 3 4 3 2 1 2 1 5 1 8 1 2 3 1 5 1 2 3 3 1 3 2 1 
 5 7 3 2 1 2 4 3 2 3 1 2 3 1 3 3 3 2 3 1 3 2 3 1 5 
 6 1 2 1 5 1 3 2 1 2 3 3 6 1 3 2 7 2 1 2 1 6 3 2 3 
 3 3 1 3 3 2 1 2 3 1 3 3 2 6 1 6 2 1 2 3 1 3 2 1 2 
 3 4 3 2 1 3 2 3 4 2 1 2 1 2 7 2 4 5 1 3 5 1 2 3 1 
 5 1 2 1 6 5 1 2 1 2 3 4 2 3 3 3 1 3 2 1 3 5 4 2 1 
 2 3 4 3 5 1 2 3 1 3 3 2 1 2 3 1 5 1 3 5 1 5 1 2 1 
 2 7 2 1 2 6 1 3 2 1 3 2 3 6 1 3 2 4 3 2 3 1 2 3 1 
 3 5 1 2 3 1 3 2 1 2 1 5 6 1 2 3 3 1 3 3 2 6 1 3 2 
 1 5 3 4 2 1 3 2 4 3 5 1 2 3 7 2 1 5 1 3 2 1 2 1 6 
 5 1 2 1 2 4 3 2 1 2 3 3 1 3 2 4 2 3 4 2 3 1 2 4 3 
 2 3 3 3 1 3 3 2 1 2 3 4 2 1 2 1 5 1 5 1 3 5 1 3 2 
 1 2 3 3 4 2 1 3 5 4 3 2 1 2 4 5 3 1 2 4 3 3 2 1 2 
 4 3 2 3 1 5 1 5 1 2 1 2 3 1 3 2 1 5 3 1 3 6 2 3 4 
 2 1 2 1 2 4 3 2 4 2 3 4 3 2 1 2 3 4 2 1 2 1 5 1 5 
 1 2 3 3 1 5 1 2 3 4 3 3 3 2 3 6 3 1 2 4 3 2 3 1 2 
 4 3 3 2 3 3 1 3 3 2 1 5 1 5 1 3 2 3 1 3 2 6 3 1 3 
 2 1 3 2 3 4 2 1 2 1 9 2 3 1 2 3 1 6 2 1 6 3 2 1 2 
 6 1 5 3 1 2 3 1 3 3 2 3 3 1 5 1 5 3 4 3 2 1 2 4 3 
 2 3 1 2 3 1 3 3 3 2 3 1 3 2 1 3 5 6 3 1 5 1 3 2 1 
 2 3 3 1 5 1 3 2 7 2 1 2 1 2 4 3 2 3 1 5 1 3 3 2 3 
 3 1 3 2 1 2 6 1 6 2 1 2 3 1 5 1 2 3 3 1 3 2 1 3 2 
 7 2 1 2 1 2 7 2 3 1 5 1 3 3 3 2 3 1 5 3 1 6 5 1 2 
 1 2 3 1 3 2 3 3 3 4 2 1 3 2 3 4 2 1 2 7 3 5 3 3 1 
 3 3 2 1 2 3 1 3 3 3 5 1 5 1 2 1 2 3 4 2 1 2 7 3 2 
 1 3 2 3 6 1 2 1 2 4 3 2 4 2 3 1 3 5 1 2 3 1 3 2 1 
 2 1 5 1 5 1 2 3 3 4 3 2 3 3 1 3 2 1 3 5 4 2 1 5 4 
 3 2 3 1 2 3 4 3 2 1 5 1 5 1 2 1 5 1 5 1 2 1 2 4 3 
 2 1 2 3 3 1 3 2 4 2 3 4 2 1 3 2 4 3 2 3 3 3 1 3 3 
 2 1 2 3 1 3 2 1 2 1 5 6 1 3 2 3 1 3 2 1 2 6 4 2 1 
 8 4 2 1 2 1 2 4 8 1 2 4 6 2 1 2 3 1 3 2 3 1 6 5 1 
 2 1 2 3 1 3 2 1 2 3 3 1 3 3 3 2 3 4 2 3 1 2 4 3 2 
 4 2 3 1 3 3 2 1 2 3 4 2 1 2 1 5 1 5 1 2 1 5 1 5 1 
 2 3 4 3 2 1 3 2 3 4 5 1 2 4 5 3 1 2 3 1 3 3 2 3 4 
 3 3 2 1 5 1 5 1 2 1 2 3 1 3 2 1 5 3 1 3 2 4 2 3 4 
 2 1 2 1 6 3 2 3 1 2 3 7 2 1 2 4 3 2 1 2 1 5 1 5 3 
 3 3 1 3 2 1 2 3 3 1 3 3 5 3 7 2 1 2 4 3 2 3 1 2 4 
 3 3 3 2 3 1 3 2 1 2 1 5 6 1 3 5 1 3 2 3 3 3 1 5 1 
 3 2 7 2 1 2 1 2 7 2 3 1 2 3 1 3 3 2 1 5 1 3 2 1 2 
 6 1 6 2 1 2 3 1 3 3 2 3 3 1 5 1 3 2 3 4 2 1 2 1 2 
 7 2 3 1 5 1 3 3 2 1 2 3 1 5 1 3 6 5 3 1 2 3 1 3 2 
 3 3 3 1 3 2 1 3 2 3 4 2 1 2 3 4 3 5 1 5 1 3 3 2 3 
 3 1 3 2 1 3 5 1 6 2 1 2 3 6 1 2 6 1 3 2 1 3 2 9 1 
 2 1 2 4 3 2 3 1 2 3 1 3 6 2 3 1 3 2 3 1 5 1 5 1 2 
 3 3 1 3 3 2 3 3 4 2 1 3 2 3 4 2 1 3 6 3 2 3 3 3 4 
 3 2 1 5 1 3 3 2 1 5 1 5 1 2 1 2 4 3 2 1 2 3 4 3 2 
 4 2 3 4 2 1 2 1 2 4 3 2 6 3 1 3 5 1 2 3 1 3 2 1 2 
 1 5 1 5 1 3 2 3 4 2 1 2 3 3 4 2 1 3 5 4 2 1 2 3 4 
 5 3 1 2 4 3 3 2 1 2 3 1 5 3 1 5 1 5 1 2 1 2 4 3 2 
 1 2 3 3 1 3 3 3 2 3 4 2 1 3 2 4 3 2 4 2 3 1 3 3 2 
 1 2 3 4 2 1 2 1 5 6 1 2 1 2 3 1 5 1 2 7 3 2 1 5 3 
 4 2 3 1 2 4 3 5 1 2 3 1 6 2 3 3 1 3 3 2 1 6 5 1 2 
 1 2 3 1 3 2 1 5 3 1 3 2 1 3 2 3 4 2 3 1 6 3 2 3 1 
 2 3 1 6 2 1 2 7 2 1 2 1 5 1 5 3 1 5 1 3 2 1 2 3 3 
 1 3 2 1 5 3 4 3 2 1 2 4 5 3 1 2 3 1 3 3 3 2 4 3 2 
 1 2 1 5 6 1 2 1 5 1 3 2 1 5 3 1 5 4 2 7 2 1 2 1 2 
 4 3 2 3 1 2 3 4 3 2 1 2 3 1 3 2 1 2 6 1 6 2 3 3 1 
 3 2 1 2 3 3 1 3 3 3 2 3 6 1 2 1 2 7 2 3 1 6 3 3 2 
 1 2 3 1 5 1 2 1 6 5 1 3 2 3 1 3 2 3 3 3 1 3 2 1 3 
 2 3 4 2 1 2 3 7 5 1 2 3 1 3 3 2 1 5 1 3 2 1 8 1 5 
 1 2 1 2 3 4 3 2 6 1 5 1 3 2 3 6 1 2 1 2 4 3 2 3 1 
 2 3 1 3 5 1 2 3 1 3 2 1 3 5 1 5 3 3 3 1 3 3 2 3 3 
 1 3 2 1 3 2 3 4 2 1 3 2 4 3 2 3 1 5 4 3 2 6 1 3 2 
 1 2 1 5 1 6 2 1 2 4 5 1 2 3 3 1 3 2 4 2 7 2 1 2 1 
 2 4 3 2 3 3 3 1 3 3 3 2 3 1 3 2 3 1 5 1 5 1 3 2 3 
 1 3 2 1 2 3 3 4 2 1 3 5 4 2 1 2 1 6 5 3 3 4 3 3 2 
 1 2 3 1 3 5 1 5 1 5 1 2 1 2 3 1 3 2 1 2 3 4 3 3 3 
 2 3 4 2 1 2 1 2 4 3 2 4 2 3 1 3 5 1 2 3 4 2 1 2 1 
 5 1 5 1 2 1 2 3 6 1 2 3 4 3 2 1 3 5 4 2 3 3 4 3 2 
 3 1 2 3 1 3 3 2 3 3 1 6 2 1 5 1 5 1 2 1 2 4 3 2 1 
 5 3 1 3 2 1 3 2 3 4 2 1 3 6 3 2 3 1 2 3 1 6 2 1 2 
 4 3 2 1 2 1 5 6 3 1 2 3 1 3 2 1 2 6 1 3 2 1 5 3 4 
 3 2 1 2 4 3 5 1 2 3 1 6 3 2 3 1 3 2 1 2 1 11 1 2 1 
 5 1 3 2 1 2 3 3 1 5 1 3 2 7 2 3 1 2 4 3 2 3 1 2 3 
 1 3 3 2 1 2 3 4 2 1 2 6 1 6 2 1 5 1 3 2 1 2 3 3 1 
 3 2 1 3 2 3 4 3 2 1 2 9 3 1 5 1 3 3 2 1 2 4 5 1 2 
 1 6 5 1 2 1 2 3 1 3 2 6 3 1 3 2 4 2 3 4 2 1 2 3 4 
 3 5 1 2 3 4 3 2 1 2 3 1 3 2 1 3 5 1 5 1 2 3 3 4 2 
 1 2 6 1 3 3 3 2 3 6 1 2 1 2 4 3 2 3 1 2 4 3 5 1 2 
 3 1 3 2 1 2 1 5 1 5 1 5 3 1 3 5 3 3 1 3 2 1 3 2 3 
 4 2 1 3 2 7 2 3 1 2 3 4 3 2 1 5 1 3 2 1 2 6 1 5 1 
 2 1 2 4 3 3 2 3 3 1 5 4 2 3 4 2 1 2 1 2 4 3 2 3 3 
 3 1 3 3 2 1 2 3 1 3 2 1 3 5 1 5 4 2 3 1 3 2 1 2 3 
 3 4 2 1 3 5 4 2 1 2 1 2 4 5 3 1 6 3 3 2 3 3 1 3 2 
 3 1 5 1 6 2 1 2 3 1 5 1 2 3 3 1 3 3 3 2 7 2 1 2 1 
 2 4 3 2 4 2 3 1 3 3 3 2 3 4 2 3 1 5 1 5 1 2 1 2 3 
 1 5 1 2 3 7 2 1 3 2 3 4 2 3 1 6 3 2 3 3 3 1 3 3 2 
 3 3 1 3 3 2 1 5 1 5 1 2 1 2 3 1 3 2 1 5 4 3 2 1 3 
 2 3 4 2 1 2 1 6 3 2 4 2 3 1 8 1 2 4 3 2 1 2 1 5 1 
 5 3 1 2 3 4 2 1 2 3 3 1 3 2 1 8 4 3 2 3 4 3 2 3 1 
 2 3 1 3 3 3 2 3 1 5 1 2 1 5 6 1 2 1 6 3 2 1 2 3 3 
 1 5 1 3 2 7 2 1 3 2 4 3 2 3 1 2 3 1 3 3 2 1 2 3 1 
 3 2 1 2 6 7 2 1 2 3 1 3 2 1 2 6 1 3 2 1 5 3 4 2 1 
 2 1 2 7 5 1 5 1 6 2 1 2 3 1 5 1 2 1 6 5 1 2 1 2 3 
 1 3 2 3 3 3 1 3 2 1 3 2 3 4 2 3 3 4 3 5 1 2 3 1 3 
 3 2 1 2 3 4 2 1 3 5 1 5 1 2 1 5 4 2 1 2 6 1 3 2 1 
 3 2 3 7 2 1 2 4 5 3 1 2 3 1 3 5 1 2 4 3 2 1 2 1 5 
 1 5 1 2 3 3 1 3 3 5 3 1 3 2 4 2 3 4 2 1 3 2 4 3 2 
 3 1 2 3 4 3 2 1 5 1 3 2 1 2 1 5 1 5 1 2 3 4 3 2 1 
 2 3 3 1 3 6 2 3 6 1 2 1 2 4 3 2 3 3 4 3 3 2 1 2 3 
 1 3 2 1 2 1 5 1 5 1 3 2 3 1 3 2 3 3 3 4 2 1 3 5 4 
 2 1 2 1 2 9 3 1 2 4 3 3 2 1 5 1 3 2 3 6 1 5 1 2 1 
 2 3 1 3 3 2 3 3 1 6 3 2 3 4 2 1 2 1 2 4 3 2 4 2 3 
 1 3 3 2 1 2 3 4 2 1 3 5 1 5 3 1 2 3 1 5 1 2 3 4 3 
 2 1 3 2 3 4 2 3 1 2 4 3 2 3 1 5 1 3 3 2 3 3 1 3 3 
 2 1 5 1 6 2 1 2 3 1 5 1 5 3 1 3 2 1 3 2 7 2 1 2 1 
 6 3 2 3 1 2 3 1 6 3 2 4 3 2 3 1 5 1 5 3 1 2 3 1 3 
 2 1 2 3 3 4 2 1 5 3 4 3 2 1 6 3 2 3 3 3 1 3 3 3 2 
 3 1 3 3 2 1 5 6 1 2 1 5 1 3 2 1 2 3 4 5 1 3 2 7 2 
 1 2 1 2 4 3 2 4 2 3 1 3 5 1 2 3 1 3 2 1 2 6 1 6 2 
 1 2 3 4 2 1 2 3 3 1 3 2 1 3 5 4 2 1 2 3 7 2 3 1 5 
 1 3 3 2 1 2 3 1 5 1 2 1 6 5 1 2 1 2 4 3 2 3 3 3 1 
 3 2 1 3 2 3 4 2 1 5 4 3 5 1 2 3 1 3 3 2 1 2 3 1 3 
 2 1 3 5 6 1 2 1 2 3 4 2 1 2 6 1 3 2 1 5 3 6 1 2 1 
 2 4 3 5 1 2 3 1 8 1 2 3 1 3 2 1 2 1 6 5 1 2 3 3 1 
 3 3 2 3 3 1 3 2 1 3 2 3 4 2 4 2 4 3 2 3 1 2 3 4 3 
 2 1 5 4 2 1 2 1 5 1 5 1 2 1 6 3 2 1 2 3 3 1 3 2 4 
 2 3 4 3 2 1 2 4 5 3 3 3 1 3 3 2 1 2 4 3 2 1 2 1 5 
 1 5 1 3 2 3 1 3 2 1 5 3 4 2 4 5 4 2 1 2 1 2 4 5 3 
 1 2 7 3 2 1 2 3 1 3 2 3 1 5 1 5 1 2 3 3 1 3 2 1 2 
 3 3 1 3 3 3 2 3 6 1 2 1 2 4 3 2 4 2 4 3 3 2 1 2 3 
 4 2 1 2 1 5 1 5 1 3 2 3 1 5 3 3 4 3 2 1 3 2 3 4 2 
 3 1 2 7 2 3 1 2 3 1 3 3 2 6 1 3 3 2 6 1 5 1 2 1 2 
 3 1 3 3 5 3 1 5 1 3 2 3 4 2 1 2 1 6 3 2 3 1 2 3 1 
 6 2 1 2 4 3 2 1 3 5 1 5 3 1 2 3 1 3 2 1 2 3 3 1 3 
 2 1 5 3 4 3 2 1 2 4 3 2 3 1 5 1 3 3 5 3 1 3 2 1 2 
 1 5 7 2 1 5 1 5 1 2 3 3 1 5 1 3 2 7 2 1 2 1 2 4 3 
 2 3 1 2 3 1 3 3 3 2 3 1 3 2 3 6 1 6 2 1 2 3 1 3 2 
 1 2 3 3 4 2 1 3 2 3 4 2 1 2 1 9 2 3 6 1 3 3 2 1 2 
 3 1 6 2 1 6 5 1 2 1 2 3 1 3 2 3 3 4 3 2 1 3 2 3 4 
 2 1 2 3 4 3 6 2 3 1 3 5 1 2 3 1 3 2 1 3 5 1 5 1 2 
 1 2 3 4 2 1 2 6 1 3 2 1 3 5 6 1 2 3 4 3 2 3 1 2 3 
 1 3 5 1 2 3 1 5 1 2 1 5 1 5 1 2 3 4 3 3 2 3 3 1 3 
 2 1 3 2 3 4 2 1 3 2 4 3 2 3 1 2 3 4 3 2 1 5 1 3 2 
 1 2 1 5 6 1 2 1 2 4 3 2 1 2 6 1 3 2 6 3 4 2 1 2 1 
 2 4 3 5 3 3 1 6 2 1 2 3 1 3 2 1 2 1 6 5 1 3 2 3 1 
 3 2 1 2 3 3 4 2 1 3 5 4 2 3 1 2 4 5 3 1 2 4 3 3 2 
 1 2 3 4 2 3 1 5 1 5 1 2 1 5 1 3 2 1 2 3 3 1 3 3 3 
 2 3 4 3 2 1 2 4 5 4 2 3 1 3 3 2 1 2 7 2 1 2 1 5 1 
 5 1 2 1 2 3 1 5 1 5 4 3 2 4 2 3 4 2 3 1 2 4 3 2 3 
 1 2 3 4 3 2 3 3 1 3 3 2 1 5 1 5 1 2 3 3 1 3 2 1 5 
 3 1 3 3 3 2 3 6 1 2 1 6 3 2 3 1 2 4 6 2 1 2 4 3 2 
 1 2 1 5 1 5 4 2 3 1 3 2 3 3 3 1 3 2 1 5 3 4 3 2 1 
 2 7 2 3 1 2 3 1 3 3 3 5 1 3 2 1 2 6 6 1 2 1 5 1 3 
 3 2 3 3 1 5 1 3 2 7 2 1 2 1 2 4 3 2 3 1 2 3 1 3 3 
 2 1 2 3 1 3 2 1 8 1
))


(defclass sieve-state ()
  ((maxints :initarg :maxints
            :type nonneg-fixnum
            :accessor sieve-state-maxints)

   (a       :initarg :a
            :type simple-array
            :accessor sieve-state-a)))


(defun create-sieve (maxints)
  (declare (nonneg-fixnum maxints))
  (make-instance 'sieve-state
    :maxints maxints
    :a (make-array
         (1+ (floor (floor maxints +bits-per-word+) 2))
         :element-type 'sieve-element-type
         :initial-element 0)))


(defun nth-bit-set-p (a n)
  (declare (type sieve-array-type a)
           (type nonneg-fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (nonneg-fixnum q) (type sieve-bitpos-type r))
    (logbitp r (aref a q))))


(defun set-nth-bit (a n)
  (declare (type sieve-array-type a)
           (type nonneg-fixnum n))
  (multiple-value-bind (q r) (floor n +bits-per-word+)
    (declare (nonneg-fixnum q) (type sieve-bitpos-type r))
    (setf #1=(aref a q)
         (logior #1# (expt 2 r)))) 0)


(defun run-sieve (sieve-state steps)
  (declare (sieve-state sieve-state) (simple-vector steps))

  (do* ((maxints (sieve-state-maxints sieve-state))
        (maxintsh (floor maxints 2))
        (a (sieve-state-a sieve-state))
        (q (1+ (isqrt maxints)))
        (step 1  (if (>= step 5759) 0 (1+ step)))
        (inc (aref steps step) (aref steps step))
        (factorh (floor 17 2))
        (qh (floor q 2)))
       ((> factorh qh) sieve-state)
    (declare (nonneg-fixnum maxints maxintsh q step inc factorh qh)
             (type sieve-array-type a))
    (unless (nth-bit-set-p a factorh)
      (do* ((istep step (if (>= istep 5759) 0 (1+ istep)))
            (ninc (aref steps istep) (aref steps istep))
            (factor (1+ (the nonneg-fixnum (* factorh 2))))
            (i (floor (the nonneg-fixnum (* factor factor)) 2)))
           ((>= i maxintsh))
        (declare (nonneg-fixnum istep ninc factor i))
        (set-nth-bit a i)
        (incf i (the nonneg-fixnum (* factor ninc)))))

    (incf factorh inc)))


(defun count-primes (sieve-state)
  (declare (sieve-state sieve-state))
  (when *list-to* (princ "2, 3, 5, 7, 11, 13, " *error-output*))
  (do* ((maxints (sieve-state-maxints sieve-state))
        (a (sieve-state-a sieve-state))
        (ncount 6)
        (factor 17)
        (step 1  (if (>= step 5759) 0 (1+ step)))
        (inc (* (aref +steps+ step) 2) (* (the nonneg-fixnum (aref +steps+ step)) 2)))
       ((> factor maxints)
        (when *list-to*
          (when (< *list-to* (sieve-state-maxints sieve-state))
            (princ "..." *error-output*))
          (terpri *error-output*))
        ncount)
     (declare (nonneg-fixnum maxints ncount factor inc)
              (type sieve-array-type a))
     (unless (nth-bit-set-p a (floor factor 2))
       (incf ncount)
       (when (and *list-to* (<= factor *list-to*))
         (format *error-output* "~d, " factor)))
     (incf factor inc)))


(defun validate (sieve-state)
  (let ((hist (cdr (assoc (sieve-state-maxints sieve-state) +results+ :test #'=))))
    (if (and hist (= (count-primes sieve-state) hist)) "yes" "no")))


(let* ((passes 0)
       (start (get-internal-real-time))
       (end (+ start (* internal-time-units-per-second 5)))
       result)
  (declare (nonneg-fixnum passes))

  (do () ((>= (get-internal-real-time) end))
    (setq result (create-sieve 1000000))
    (run-sieve result +steps+)
    (incf passes))

  (let* ((duration  (/ (- (get-internal-real-time) start) internal-time-units-per-second))
         (avg (/ duration passes)))
    (format *error-output* "Algorithm: wheel optimized  Passes: ~d, Time: ~f, Avg: ~f ms, Count: ~d  Valid: ~A~%"
            passes duration (* 1000 avg) (count-primes result) (let ((*list-to* nil)) (validate result)))

    (format t "mayerrobert-cl-wheel-opt;~d;~f;1;algorithm=wheel,faithful=no,bits=1~%" passes duration)))
