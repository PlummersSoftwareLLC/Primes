#!/usr/bin/env swipl

:- initialization(main, main).

main(Argv) :-
  (maplist(term_string, [N], Argv) ; N = 1_000_000),
  statistics(walltime, [T0|_]),
  time_primes(N, 5000, Iters),
  statistics(walltime, [T1|_]),
  format(
    "jimbxb_prolog;~d;~f;1;algorithm=base,faithful=yes,bits=1~n", 
    [Iters, (T1 - T0) / 1000]
  ),
  true.

time_primes(_, RemT, 0) :-
  RemT < 0,
  !.
time_primes(N, RemT, I0+1) :-
  statistics(walltime, [T0|_]),
  primes(N, Ps),
  length(Ps, Count),
  prime_count(N, Count),
  statistics(walltime, [T1|_]),
  RemT1 is RemT - T1 + T0,
  time_primes(N, RemT1, I0).

prime_count(N, Count) :-
  KnownCounts = [
    10-4,
    100-25,
    1000-168,
    10000-1229,
    100000-9592,
    1000000-78498,
    10000000-664579,
    100000000-5761455
  ],
  ( member(N-Count, KnownCounts)
  ; \+ member(N-_, KnownCounts)
  ),
  !.

primes(N, Ps) :-
  ( N < 2 -> Ps = []
  ; N < 3 -> Ps = [2]
  ; sieve(N, 3, 0, Ps1), Ps = [2|Ps1]
  ).

sieve(N, P0, Bs0, [P0|Ps1]) :-
  P0 =< N,
  0 is getbit(Bs0, P0 div 2),
  !,
  Start is P0 * P0,
  Skip is P0 * 2,
  setbits(N, Skip, Start, Bs0, Bs1),
  P1 is P0 + 2,
  sieve(N, P1, Bs1, Ps1).
sieve(N, P0, Bs, Ps) :-
  P0 =< N,
  !,
  P1 is P0 + 2,
  sieve(N, P1, Bs, Ps).
sieve(_, _, _, []).

setbits(N, Skip, M0, Bs0, Bs) :-
  ( M0 > N ->
    Bs = Bs0
  ; M1 is M0 + Skip,
    Bs1 is Bs0 \/ (1 << (M0 div 2)), 
    setbits(N, Skip, M1, Bs1, Bs)
  ).