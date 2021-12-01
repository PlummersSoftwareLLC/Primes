defmodule PrimeSieve do
  @moduledoc """
  PrimeSieve with uses bits to store primes
  """

  @run_for_seconds 5
  @expected %{
    10 => 4,
    100 => 25,
    1000 => 168,
    10000 => 1229,
    100_000 => 9592,
    1_000_000 => 78498,
    10_000_000 => 664_579,
    100_000_000 => 5_761_455
  }

  defguard is_even(integer) when rem(integer, 2) == 0

  @doc """
  Generate a sieve with all ones
  """
  def create_sieve(size) do
    size = div2(size)
    <<-1::size(size)>>
  end

  @doc """
  Count the primes in the sieve
  """
  def count_primes(<<_::size(1), rest::bits>>) do
    # skip the first '1'
    # have 2 set as a prime (so start with one prime already counted)
    do_count_primes(rest, 1)
  end

  defp do_count_primes(<<0, rest::bits>>, acc) do
    do_count_primes(rest, acc)
  end

  defp do_count_primes(<<>>, acc) do
    acc
  end

  defp do_count_primes(<<0::size(1), rest::bits>>, acc) do
    do_count_primes(rest, acc)
  end

  defp do_count_primes(<<1::size(1), rest::bits>>, acc) do
    do_count_primes(rest, acc + 1)
  end

  @doc """
  Get one bit from the sieve
  """
  def get_bit(_, at) when is_even(at), do: 0
  def get_bit(sieve, at) when not is_even(at), do: get_real_bit(sieve, div2(at))

  defp get_real_bit(sieve, at) do
    pos = Bitwise.bsr(at, 3)
    bit_pos = pos |> Bitwise.bsl(3) |> Bitwise.bxor(at)

    if pos == 0 do
      <<_::size(bit_pos), bit::size(1), _::bits>> = sieve
      bit
    else
      <<_::size(bit_pos), bit::size(1), _::bits>> = :binary.part(sieve, pos, 1)
      bit
    end
  end

  @doc """
  Clear one bit from the sieve
  """
  def clear_bit(sieve, at) when is_even(at), do: sieve
  def clear_bit(sieve, at) when not is_even(at), do: clear_real_bit(sieve, div2(at))

  defp clear_real_bit(sieve, at) do
    case sieve do
      <<_::size(at), 0::size(1), _::bits>> ->
        sieve

      <<pre::size(at), 1::size(1), post::bits>> ->
        <<pre::size(at), 0::size(1), post::bits>>
    end
  end

  @doc """
  Clear bits from the sieve, with the given step
  """
  def clear_bits(sieve, step) do
    start_at = div(3 * step, 2)

    case sieve do
      <<start::size(start_at), tail::bits>> ->
        do_clear_bits(tail, step, [<<start::size(start_at)>>])

      sieve ->
        sieve
    end
  end

  defp do_clear_bits(<<>>, _step, acc) do
    acc |> :lists.reverse() |> :erlang.list_to_bitstring()
  end

  defp do_clear_bits(<<1::size(1), post::bits>>, step, acc) do
    new_step = step - 1

    case post do
      <<pre::size(new_step), tail::bits>> ->
        do_clear_bits(tail, step, [<<0::size(1), pre::size(new_step)>> | acc])

      post ->
        do_clear_bits(<<>>, step, [<<0::size(1), post::bits>> | acc])
    end
  end

  defp do_clear_bits(post, step, acc) do
    case post do
      <<pre::size(step), tail::bits>> ->
        do_clear_bits(tail, step, [<<pre::size(step)>> | acc])

      post ->
        do_clear_bits(<<>>, step, [post | acc])
    end
  end

  defp div2(integer), do: div(integer, 2)

  @doc """
  Find the 'next' factor or next prime
  """
  def next_factor(sieve, from), do: do_next_factor(sieve, from)

  defp do_next_factor(sieve, from) do
    # we can probably do some fancy pattern matching, similar what we do for count_primes, for now just use the get bit function
    if get_bit(sieve, from) == 1 do
      from
    else
      do_next_factor(sieve, from + 1)
    end
  end

  @doc """
  Find all the primes uptill `size`
  """
  def create_prime_sieve(size) do
    sieve = create_sieve(size)
    till = size |> :math.sqrt() |> :erlang.floor()
    {sieve, _} = Enum.reduce_while(2..div(till, 2), {sieve, 3}, &run_reduce(&1, &2, till))
    sieve
  end

  @doc """
  Find all the primes uptill `size` and return the amount of primes
  """
  def run(size) do
    size
    |> create_prime_sieve()
    |> count_primes()
  end

  defp run_reduce(_, {sieve, factor}, till) do
    if factor <= till do
      factor = next_factor(sieve, factor)
      sieve = clear_bits(sieve, factor)
      {:cont, {sieve, factor + 2}}
    else
      {:halt, {sieve, factor}}
    end
  end

  @doc """
  Start the process
  """
  def start(size, start_time) do
    pass(size, start_time, 0)
  end

  defp pass(size, start_time, pass_count) do
    if exceeded_time_threshold?(start_time) do
      pass_count
    else
      results = run(size)
      ^results = @expected[size]
      pass(size, start_time, pass_count + 1)
    end
  end

  def exceeded_time_threshold?(start_time) do
    Time.diff(Time.utc_now(), start_time, :second) > @run_for_seconds
  end

  @doc """
  Main function
  """
  def main do
    sieve_size = 1_000_000
    start_time = Time.utc_now()
    pass_count = start(sieve_size, start_time)
    duration = Time.diff(Time.utc_now(), start_time, :microsecond)

    IO.puts(
      "thomas9911;#{pass_count};#{Float.round(duration / 1_000_000, 3)};1;algorithm=base,faithful=yes,bits=1"
    )
  end

  @doc """
  Return the sieve as a list
  """
  def print_sieve(sieve), do: do_print_sieve(sieve, [])

  @doc """
  Return all the primes stored in the sieve
  """
  def print_primes(sieve) do
    sieve
    |> print_sieve()
    |> swap_one_and_two()
    |> Enum.with_index(1)
    |> Enum.filter(fn
      {0, _} -> false
      {1, _} -> true
    end)
    |> Enum.map(&elem(&1, 1))
  end

  defp swap_one_and_two([1, 0 | tail]) do
    [0, 1 | tail]
  end

  defp do_print_sieve(<<>>, acc) do
    acc
    |> :lists.reverse()
    |> Enum.intersperse(0)
  end

  defp do_print_sieve(<<a::size(1), rest::bits>>, acc) do
    do_print_sieve(rest, [a | acc])
  end
end
