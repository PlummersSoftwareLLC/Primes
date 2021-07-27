#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :-
  (maplist(term_string, [N], Argv) ; N = 1_000_000),
  statistics(walltime, [T0|_]),
  timed_primes(N, 5000, Iters),
  statistics(walltime, [T1|_]),
  format(
    "jimbxb-prolog-basic;~d;~f;1;algorithm=base,faithful=yes,bits=1~n", 
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
  ; sieve(bitvector(N, 0), 3, Ps1), Ps = [2|Ps1]
  ).

sieve(State0, P0, [P0|Ps1]) :-
  bitvector(N, Bs0) = State0,
  P0 =< N,
  0 is getbit(Bs0, P0 div 2 - 1),
  !,
  Start is P0 * P0,
  Skip is P0 * 2,
  setbits(Skip, Start, State0, State1),
  P1 is P0 + 2,
  sieve(State1, P1, Ps1).
sieve(State, P0, Ps) :-
  bitvector(N, _) = State, 
  P0 =< N,
  !,
  P1 is P0 + 2,
  sieve(State, P1, Ps).
sieve(_, _, []).

setbits(Skip, M0, State0, State1) :-
  bitvector(N, Bs0) = State0,
  ( M0 > N ->
    State1 = State0
  ; M1 is M0 + Skip,
    Bs1 is Bs0 \/ (1 << (M0 div 2 - 1)), 
    setbits(Skip, M1, bitvector(N, Bs1), State1)
  ).
