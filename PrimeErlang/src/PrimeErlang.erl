%% -*- erlang -*-

-module('PrimeErlang').

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

-mode(compile).

num_primes(10) ->
  4;
num_primes(100) ->
  25;
num_primes(1_000) ->
  168;
num_primes(10_000) ->
  1229;
num_primes(100_000) ->
  9592;
num_primes(1_000_000) ->
  78498;
num_primes(10_000_000) ->
  664579;
num_primes(100_000_000) ->
  5761455;
num_primes(1_000_000_000) ->
  50847534;
num_primes(10_000_000_000) ->
  455052511.

main([Arg]) ->
  N = list_to_integer(Arg),
  {Time, Primes} = timer:tc(fun() -> run_sieve(N) end),
  ?assertEqual(Primes, num_primes(N)),
  io:format("Time: ~p msecs~n", [Time / 1000]).

%% -- Implementation --

run_sieve(N) ->
  Q = math:sqrt(N),
  A = bits_new(N),
  run_sieve_loop(2, Q, N, A),
  count_primes(2, N, A, 0).

run_sieve_loop(I, Q, _, _) when I > Q ->
  ok;
run_sieve_loop(I, Q, N, A) ->
  case bits_is_prime(I, A) of
    true ->
      run_sieve_inner_loop(I * I, I, N, A);
    false ->
      ok
  end,
  run_sieve_loop(I + 1, Q, N, A).

run_sieve_inner_loop(J, _, N, A) when J > N ->
  A;
run_sieve_inner_loop(J, Incr, N, A) ->
  run_sieve_inner_loop(J + Incr, Incr, N, bits_set_not_prime(J, A)).

count_primes(I, N, _, Acc) when I > N ->
  Acc;
count_primes(I, N, A, Acc) ->
  count_primes(I + 1, N, A, Acc + bits_value(I, A)).

bits_new(N) ->
  atomics:new(N, []).

bits_value(N, Bits) ->
  V = atomics:get(Bits, N),
  if V == 0 ->
      1;
     true ->
      0
  end.

bits_is_prime(N, Bits) ->
  atomics:get(Bits, N) == 0.

bits_set_not_prime(N, Bits) ->
  ok = atomics:put(Bits, N, 1),
  Bits.
