#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :-
  (maplist(term_string, [N], Argv) ; N = 1_000_000),
  statistics(walltime, [T0|_]),
  timed_primes(N, 5000, Iters),
  statistics(walltime, [T1|_]),
  format(
    "jimbxb_prolog;~d;~f;1;algorithm=base,faithful=yes,bits=1~n", 
    [Iters, (T1 - T0) / 1000]
  ),
  true.

timed_primes(_, RemT, 0) :-
  RemT < 0,
  !.
timed_primes(N, RemT, I0+1) :-
  statistics(walltime, [T0|_]),
  primes(N, Ps),
  length(Ps, Count),
  prime_count(N, Count),
  statistics(walltime, [T1|_]),
  RemT1 is RemT - T1 + T0,
  timed_primes(N, RemT1, I0).

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
  ; sieve(state(N, 0), 3, Ps1), Ps = [2|Ps1]
  ).

sieve(state(N, Bs0), P0, [P0|Ps1]) :-
  P0 =< N,
  0 is getbit(Bs0, P0 div 2),
  !,
  Start is P0 * P0,
  Skip is P0 * 2,
  setbits(N, Skip, Start, Bs0, Bs1),
  P1 is P0 + 2,
  sieve(state(N, Bs1), P1, Ps1).
sieve(state(N, Bs), P0, Ps) :-
  P0 =< N,
  !,
  P1 is P0 + 2,
  sieve(state(N, Bs), P1, Ps).
sieve(_, _, []).

setbits(N, Skip, M0, Bs0, Bs) :-
  ( M0 > N ->
    Bs = Bs0
  ; M1 is M0 + Skip,
    Bs1 is Bs0 \/ (1 << (M0 div 2)), 
    setbits(N, Skip, M1, Bs1, Bs)
  ).