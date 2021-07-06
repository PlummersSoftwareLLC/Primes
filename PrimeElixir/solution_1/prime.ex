defmodule PrimeSieve do

  @reference_results %{
      10 => 4,
      100 => 25,
      1000 => 168,
      10000 => 1229,
      100000 => 9592,
      1000000 => 78498,
      10000000 => 664579,
      100000000 => 5761455
  }

  @doc """
  Count true values in list
  """
  def count_primes(prime_list) do
    Enum.count(prime_list, fn x -> x end)
  end

  def run_sieve(prime_list,factor, sieve_size) do
    sieve_sqrt = :math.sqrt(sieve_size)
    # Use recursion instead of while loop
    if factor >= sieve_sqrt do
      prime_list
    else
      new_prime_list = clear_multiples(prime_list, factor)
      run_sieve(new_prime_list, factor + 2, sieve_size)
    end
  end

  # Returns index of start of true
  def find_start_of_list(prime_list, factor, sieve_size) do
    Enum.find_index(prime_list, fn x -> x end)
  end

  def clear_multiples(prime_list, factor) do
    range_bottom = factor * 3
    # range  = (factor * 3)..sieve_size//(factor *2)
    # Split the list
    {left_primes, right_primes} = Enum.split(prime_list, range_bottom)
    # Convert the multiple by stepping through the list
    converted_list = Enum.map_every(right_primes, (factor * 2), fn x -> false end)
    # join them back together and return
    Enum.concat(left_primes, converted_list)
  end

  def validate_results(sieve_size, count, reference) do
    Map.get(reference, sieve_size) == count
  end

  def start(prime_list, sieve_size, start_time) do
    pass(prime_list, sieve_size, start_time, 0)
  end

  def pass(prime_list, sieve_size, start_time, pass_count) do
    if Time.diff(Time.utc_now(), start_time, :second) > 5 do
      pass_count
    else
      results = run_sieve(prime_list, 3, sieve_size)
      validate_results(sieve_size, results, @reference_results)
      pass(prime_list, sieve_size, start_time, pass_count + 1)
    end
  end
end

# Setup
size = 1000000
primes = Enum.map(1..size, fn(_x) -> true end)

# Run
start_time = Time.utc_now()
pass_count = PrimeSieve.start(primes, size, start_time)
duration  = Time.diff(Time.utc_now(), start_time, :microsecond)

# Print
IO.puts "cdesch;#{pass_count};#{Float.round(duration/1000000, 3)};1;algorithm=base,faithful=no"
