%% -*- erlang -*-
%%
%% Sieve of Erastothenes in Erlang, implemented as a benchmark for
%% https://github.com/PlummersSoftwareLLC/Primes.
%%
%% @author Jesper Eskilson <jesper@eskilson.se>
%%
-module('PrimeErlang').

-export([main/1, run_benchmark/1]).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================
%% Types
%% ============================================================

-record(sieve, {q :: float(), size :: integer(), bits :: atomics:atomics_ref()}).

-type sieve() :: #sieve{}.
-type prime_idx() :: pos_integer().

%% ============================================================
%% Exported API
%% ============================================================

%% @doc The escript entry point
main([]) ->
  main(["1000000"]);
main([Arg]) ->
  N = list_to_integer(Arg),
  run_benchmark(N).

%% @doc Run the benchmark for a sieve of the given size.
run_benchmark(N) ->
  ExpectedNumPrimes = num_primes(N),
  LimitSecs = 5,

  BenchFun =
    fun() ->
       NumPrimes = run_sieve(N),
       ?assertEqual(NumPrimes, ExpectedNumPrimes)
    end,

  {TimeUsecs, Iterations} = timer:tc(fun() -> repeat_until(BenchFun, LimitSecs) end),

  TotalTime = TimeUsecs / 1_000_000.0,
  NumThreads = 1,
  Label = "jesperes",
  Tags =
    #{algorithm => base,
      faithful => yes,
      bits => 64},

  print_result(Label, Iterations, TotalTime, NumThreads, Tags).

%% ============================================================
%% Implementation
%% ============================================================

%% @doc Run the sieve for the given size once, and return the number
%% of primes found.
-spec run_sieve(Size :: integer()) -> NumPrimes :: integer().
run_sieve(Size) ->
  Sieve = sieve_new(Size),
  Sieve0 = run_sieve_loop(2, Sieve),
  sieve_count_primes(Sieve0).

-spec run_sieve_loop(I :: prime_idx(), Sieve :: sieve()) -> sieve().
run_sieve_loop(I, #sieve{q = Q} = Sieve) when I > Q ->
  Sieve;
run_sieve_loop(I, Sieve) ->
  Sieve0 =
    case sieve_is_prime(I, Sieve) of
      true ->
        run_sieve_inner_loop(I * I, I, Sieve);
      false ->
        Sieve
    end,
  run_sieve_loop(I + 1, Sieve0).

-spec run_sieve_inner_loop(J :: prime_idx(), Incr :: integer(), Sieve :: sieve()) ->
                            sieve().
run_sieve_inner_loop(J, _, #sieve{size = Size} = Sieve) when J > Size ->
  Sieve;
run_sieve_inner_loop(J, Incr, Sieve) ->
  Sieve0 = sieve_mark_not_prime(J, Sieve),
  run_sieve_inner_loop(J + Incr, Incr, Sieve0).

%% ============================================================
%% Sieve
%% ============================================================

-spec sieve_new(Size :: integer()) -> sieve().
sieve_new(Size) ->
  #sieve{q = math:sqrt(Size),
         size = Size,
         bits = atomics:new(Size, [])}.

-spec sieve_is_prime(N :: prime_idx(), Sieve :: sieve()) -> boolean().
sieve_is_prime(N, Sieve) ->
  atomics:get(Sieve#sieve.bits, N) == 0.

-spec sieve_mark_not_prime(N :: prime_idx(), Sieve :: sieve()) -> sieve().
sieve_mark_not_prime(N, Sieve) ->
  ok = atomics:put(Sieve#sieve.bits, N, 1),
  Sieve.

-spec sieve_count_primes(Sieve :: sieve()) -> integer().
sieve_count_primes(Sieve) ->
  sieve_count_primes(2, 0, Sieve).

sieve_count_primes(I, Acc, #sieve{size = Size}) when I > Size ->
  Acc;
sieve_count_primes(I, Acc, Sieve) ->
  case atomics:get(Sieve#sieve.bits, I) of
    0 ->
      sieve_count_primes(I + 1, 1 + Acc, Sieve);
    1 ->
      sieve_count_primes(I + 1, Acc, Sieve)
  end.

%% ============================================================
%% Helpers
%% ============================================================

%% @doc Used to verify the result
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

%% @doc Print the result
print_result(Label, Iterations, TotalTime, NumThreads, Tags) ->
  io:format("~s;~w;~g;~w;~s~n",
            [Label, Iterations, TotalTime, NumThreads, tags_to_str(Tags)]).

%% @doc Convert tags on map-format to a comma-separated k=v string
tags_to_str(Tags) ->
  lists:join(",",
             lists:map(fun({K, V}) -> io_lib:format("~w=~w", [K, V]) end, maps:to_list(Tags))).

%% @doc Run a function repeatedly for a given number of
%% seconds. Return the number of times the function was run.
-spec repeat_until(Fun :: fun(), LimitSecs :: pos_integer()) ->
                    Iterations :: pos_integer().
repeat_until(Fun, LimitSecs) ->
  Start = os:system_time(),
  Limit = erlang:convert_time_unit(LimitSecs, second, native),
  repeat_until(Fun, Start, Limit, 0).

repeat_until(Fun, Start, Limit, NIters) ->
  Elapsed = os:system_time() - Start,
  if Elapsed >= Limit ->
       NIters;
     true ->
       Fun(),
       repeat_until(Fun, Start, Limit, NIters + 1)
  end.
