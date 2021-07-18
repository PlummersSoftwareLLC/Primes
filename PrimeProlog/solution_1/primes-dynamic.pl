#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :-
  (maplist(term_string, [N], Argv) ; N = 1_000_000), 
  !,
  statistics(walltime, [T0|_]),
  timed_primes(N, 5000, Iters),
  statistics(walltime, [T1|_]),
  format(
    "jimbxb-prolog-dynamic;~d;~f;1;algorithm=base,faithful=no~n", 
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

:- dynamic composite/1.

primes(N, Ps) :-
  ( N < 2 -> Ps = []
  ; N < 3 -> Ps = [2]
  ; retractall(composite(_)), sieve(N, 3, Ps1), Ps = [2|Ps1]
  ).

sieve(N, P0, Ps) :-
  P0 =< N,
  !,
  ( composite(P0)
  ->
    Ps = Ps0 
  ; 
    assert_composites(N, P0),
    Ps = [P0|Ps0]
  ),
  P1 is P0 + 2,
  sieve(N, P1, Ps0).
sieve(_, _, []).

assert_composites(N, P) :-
  Start is P * P,
  Skip is P * 2,
  Hi is (N - Start) div Skip,
  ( 
    between(0, Hi, X),
    C is X * Skip + Start,
    assertz(composite(C)),
    false
  ; 
    true
  ).
