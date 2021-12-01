#!/usr/bin/env bash

GS='gs -q -dNOSAFER -dNOPAUSE -dBATCH -dNODISPLAY -c'

test_primes() {
    echo -n "TEST '$1' -> '$2' "
    result="$($GS "(primesieve.ps) run $1 pstack")"
    result="$(echo $result)"
    if test "$result" = "$2"; then
        echo PASS
    else
        echo FAIL
        echo "  result was: $result"
    fi
}

test_primes '       0 countprimes ='       0
test_primes '       1 countprimes ='       0
test_primes '       2 countprimes ='       1
test_primes '       3 countprimes ='       2
test_primes '       4 countprimes ='       2
test_primes '       5 countprimes ='       3
test_primes '       6 countprimes ='       3
test_primes '       7 countprimes ='       4
test_primes '       8 countprimes ='       4
test_primes '       9 countprimes ='       4
test_primes '      10 countprimes ='       4
test_primes '     100 countprimes ='      25
test_primes '    1000 countprimes ='     168
test_primes '   10000 countprimes ='    1229
test_primes '  100000 countprimes ='    9592
test_primes ' 1000000 countprimes ='   78498
test_primes '10000000 countprimes ='  664579

test_primes '0 listprimes' ''
test_primes '1 listprimes' ''
test_primes '2 listprimes' '2'
test_primes '3 listprimes' '2 3'
test_primes '4 listprimes' '2 3'
test_primes '5 listprimes' '2 3 5'
test_primes '6 listprimes' '2 3 5'
test_primes '7 listprimes' '2 3 5 7'
test_primes '8 listprimes' '2 3 5 7'
test_primes '9 listprimes' '2 3 5 7'
