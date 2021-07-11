defmodule PrimeSieve do
  @moduledoc """
  Elixir solution by cdesch for the Primes Software Drag Race
  Reference: https://github.com/PlummersSoftwareLLC/Primes
  """
  @reference_results %{
      10 => 4,
      100 => 25,
      1000 => 168,
      10_000 => 1_229,
      100_000 => 9_592,
      1_000_000 => 78_498,
      10_000_000 => 664_579,
      100_000_000 => 5_761_455
  }
  @pass_time_threshold 5
  @default_sieve_size 1_000_000

  def main(_args) do
    # Setup
    sieve_size = @default_sieve_size
    prime_list = Enum.map(1..sieve_size, fn(_x) -> true end)

    # Run
    start_time = Time.utc_now()
    pass_count = start(prime_list, sieve_size, start_time)
    duration = Time.diff(Time.utc_now(), start_time, :microsecond)

    # Print
    IO.puts "cdesch;#{pass_count};#{Float.round(duration/1000000, 3)};1;algorithm=base,faithful=no"
  end

  @doc """
  Start the sieve with the first pass
  """
  def start(prime_list, sieve_size, start_time) do
    pass(prime_list, sieve_size, start_time, 0)
  end

  @doc """
  Recursively run passes of the sieve until time expires and count the number of passes
  """
  def pass(prime_list, sieve_size, start_time, pass_count) do
    # Check the while loop condition
    if exceeded_time_threshold?(start_time) do
      pass_count
    else
      # Run Sieve
      results = run_sieve(prime_list, 3, sieve_size)
      # Validate Results
      validate_results(results, sieve_size)
      # Move on to the next pass as part of the while loop
      pass(prime_list, sieve_size, start_time, pass_count + 1)
    end
  end

  @doc """
  Check if the pass time has exceeded the threshold
  """
  def exceeded_time_threshold?(start_time) do
    Time.diff(Time.utc_now(), start_time, :second) > @pass_time_threshold
  end

  @doc """
  Count true values in list
  """
  def count_primes(prime_list) do
    Enum.count(prime_list, fn x -> x end)
  end

  @doc """
  Run the prime sieve by recusrively increasing the factor
  until it is greater than the square root of the sieve_size
  """
  def run_sieve(prime_list, factor, sieve_size) do
    sieve_sqrt = :math.sqrt(sieve_size)
    # Use recursion instead of while loop
    if factor >= sieve_sqrt do
      prime_list
    else
      # Clear the multiples for this factor
      new_prime_list = clear_multiples(prime_list, factor)
      # Run recursively by adding 2 to the factor
      run_sieve(new_prime_list, factor + 2, sieve_size)
    end
  end

  @doc """
  Returns index of start of true
  """
  def find_start_of_list(prime_list) do
    Enum.find_index(prime_list, fn x -> x end)
  end

  @doc """
  Clear the multiples of the factor by turning them to true
  """
  def clear_multiples(prime_list, factor) do
    range_bottom = factor * 3
    # range  = (factor * 3)..sieve_size//(factor *2)
    # Split the list
    {left_primes, right_primes} = Enum.split(prime_list, range_bottom)
    # Convert the multiple by stepping through the list
    converted_list = Enum.map_every(right_primes, (factor * 2), fn _x -> false end)
    # join them back together and return
    Enum.concat(left_primes, converted_list)
  end

  @doc """
  Validate sieve results
  """
  def validate_results(results, sieve_size) do
    results
    |> Enum.drop_every(2)
    |> count_primes()
    |> validate_prime_count(sieve_size)
  end

  @doc """
  Validate the prime count for the sieve_size with the reference results
  """
  def validate_prime_count(count, sieve_size) do
    Map.get(@reference_results, sieve_size) == count
  end
end
