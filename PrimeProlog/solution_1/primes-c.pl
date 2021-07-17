#!/usr/bin/env swipl

:- initialization(main, main).

:- load_foreign_library(bitvector).

main(Argv) :-
  (maplist(term_string, [N], Argv) ; N = 1_000_000), 
  !,
  statistics(walltime, [T0|_]),
  timed_primes(N, 5000, Iters),
  statistics(walltime, [T1|_]),
  format(
    "jimbxb-prolog-c;~d;~f;1;algorithm=base,faithful=no,bits=1~n", 
    [Iters, (T1 - T0) / 1000]
  ),
  true.

timed_primes(_, RemT, 0) :-
  RemT < 0,
  !.
timed_primes(N, RemT, I1) :-
  statistics(walltime, [T0|_]),
  primes(N, Ps),
  length(Ps, Count),
  prime_count(N, Count),
  !,
  statistics(walltime, [T1|_]),
  RemT1 is RemT - T1 + T0,
  timed_primes(N, RemT1, I0),
  I1 is I0 + 1.

prime_count(10, 4).
prime_count(100, 25).
prime_count(1000, 168).
prime_count(10000, 1229).
prime_count(100000, 9592).
prime_count(1000000, 78498).
prime_count(10000000, 664579).
prime_count(100000000, 5761455).

primes(N, Ps) :-
  ( N < 2 -> Ps = []
  ; N < 3 -> Ps = [2]
  ; empty_bitvector(N, BV), sieve(BV, 3, Ps1), Ps = [2|Ps1]
  ).

sieve(BV, P0, Ps) :-
  bitvector(N, _) = BV,
  P0 =< N,
  !,
  ( bitvector_getbit(BV, P0, true)
  ->
    Ps = Ps0 
  ; 
    bitvector_setbits(BV, P0),
    Ps = [P0|Ps0]
  ),
  P1 is P0 + 2,
  sieve(BV, P1, Ps0).
sieve(_, _, []).
