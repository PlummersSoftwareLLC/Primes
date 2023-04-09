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

main([]) ->
  main(["1000000"]);
main([Arg]) ->
  N = list_to_integer(Arg),
  T0 = os:system_time(nanosecond),
  Limit = erlang:convert_time_unit(5, second, nanosecond),
  Iterations = do_main(T0, Limit, N, 0),
  T1 = os:system_time(nanosecond),
  TotalTime = (T1 - T0) / 1_000_000_000.0,
  NumThread = 1,
  Label = "jesperes",
  Tags =
    #{algorithm => base,
      faithful => yes,
      bits => 64},
  io:format("~s;~w;~g;~w;~s~n",
            [Label,
             Iterations,
             TotalTime,
             NumThread,
             lists:join(",",
                        lists:map(fun({K, V}) -> io_lib:format("~w=~w", [K, V]) end,
                                  maps:to_list(Tags)))]).

do_main(T0, Limit, N, NIter) ->
  T1 = os:system_time(nanosecond),
  Elapsed = T1 - T0,
  if Elapsed > Limit ->
       NIter;
     true ->
       NumPrimes = run_sieve(N),
       ?assertEqual(NumPrimes, num_primes(N)),
       do_main(T0, Limit, N, NIter + 1)
  end.

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
