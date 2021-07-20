#! /usr/bin/scheme --program

; import the sieve library
(import (chezscheme) (sieve))

; do as many passes as possible in under 5 seconds
(let ([start-time (current-time)]
      [five-seconds (make-time 'time-duration 0 5)]
      [maximum 1000000])
  (let loop ([passes 1])
    (sieve maximum)
    (let ([diff (time-difference (current-time) start-time)])
      (if (time>=? diff five-seconds)
        (print-results diff passes maximum)
        (loop (+ passes 1))))))
