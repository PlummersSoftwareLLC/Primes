defmodule PrimeSieveTest do
  use ExUnit.Case
  doctest PrimeSieve

  test "Count Primes" do
    primes = [true, false, true, true]
    assert PrimeSieve.count_primes(primes) == 3
  end

  test "sieve with 1_000" do
    sieve_size = 1_000
    prime_list = Enum.map(1..sieve_size, fn(_x) -> true end)

    # Run sieve
    results = PrimeSieve.run_sieve(prime_list, 3, sieve_size) # |>  Enum.drop_every(2) |> PrimeSieve.count_primes()
    # Validate Results
    assert PrimeSieve.validate_results(results, sieve_size) == true
  end

  test "sieve with 10_000" do
    sieve_size = 10_000
    prime_list = Enum.map(1..sieve_size, fn(_x) -> true end)

    # Run sieve
    results = PrimeSieve.run_sieve(prime_list, 3, sieve_size) # |>  Enum.drop_every(2) |> PrimeSieve.count_primes()
    # Validate Results
    assert PrimeSieve.validate_results(results, sieve_size) == true
  end

  test "pass" do
    # Setup
    sieve_size = 1_000
    prime_list = Enum.map(1..sieve_size, fn(_x) -> true end)

    # Run
    start_time = Time.utc_now()
    pass_count = PrimeSieve.start(prime_list, sieve_size, start_time)
    # Validate Results
    assert pass_count >= 1
  end
end
