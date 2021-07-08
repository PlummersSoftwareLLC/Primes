defmodule PrimeSieve do
  @moduledoc """
  Elixir solution by ityonemo for the Primes Software Drag Race.
  Reference: https://github.com/PlummersSoftwareLLC/Primes

  - uses :counters as a mutable array.  Since this comes with the erlang VM, note:
    - counters is 1-indexed
    - counters is mutable!  Handle with care
  """

  # 5s = 5000 ms
  @time 5000

  def main(opts \\ []) do
    size = Keyword.get(opts, :size, 1_000_000)
    threads = Keyword.get(opts, :threads, System.schedulers())

    Enum.each(1..threads, &threaded_launch(&1, size))
  end

  def threaded_launch(threads, size) do
    start_time = Time.utc_now()

    pass_count = 1..threads
    |> Enum.map(fn _ ->
      Task.async(fn ->
        :timer.send_after(@time, self(), :stop)
        run(size)
      end)
    end)
    |> Enum.reduce(0, fn future, count_so_far ->
      count_so_far + Task.await(future, 6000)
    end)

    duration = Time.diff(Time.utc_now(), start_time, :millisecond)

    # Print
    IO.puts "ityonemo;#{pass_count};#{Float.round(duration/1000, 3)};#{threads};algorithm=base,faithful=yes"
  end

  if Mix.env() == :test do
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
  end

  defmacrop validate(sieve_ast, size_ast) do
    if Mix.env() == :test do
      quote do
        size = unquote(size_ast)
        if size in Map.keys(@reference_results) do
          sieve = unquote(sieve_ast)
          count = count_primes(sieve)
          expected = @reference_results[size]
          unless count == expected do
            raise "incorrect count, expected #{expected}, got #{count}"
          end
        else
          unquote(sieve_ast)
        end
      end
    else
      sieve_ast
    end
  end

  def run(size), do: run(size, Float.round(:math.sqrt(size)), 0)
  defp run(size, prime_limit, count) do
    receive do
      :stop -> count
    after
      0 ->
        size
        |> setup_sieve
        |> filter(3, prime_limit, size)
        |> validate(size)

        run(size, prime_limit, count + 1)
    end
  end

  defp setup_sieve(size) do
    :counters.new(div(size, 2) - 1, [])
  end

  defp filter(sieve, prime, limit, _size) when prime > limit, do: sieve
  defp filter(sieve, prime, limit, size) do
    sieve
    |> tag_composites(div(prime * prime, 2), prime, div(size, 2) - 1)
    |> find_next(prime + 2, limit)
    |> case do
      int when is_integer(int) ->
        filter(sieve, int, limit, size)
      sieve -> sieve
    end
  end

  defp tag_composites(sieve, exceeded, _prime, limit) when exceeded > limit, do: sieve

  defp tag_composites(sieve, index, prime, limit) do
    :counters.add(sieve, index, 1)
    tag_composites(sieve, index + prime, prime, limit)
  end

  defp find_next(sieve, candidate, limit) when candidate > limit, do: sieve
  defp find_next(sieve, candidate, limit) do
    case :counters.get(sieve, div(candidate, 2)) do
      0 -> candidate
      _ -> find_next(sieve, candidate + 2, limit)
    end
  end

  def count_primes(sieve) do
    # needs 1 + to account for the missing value "2" (the counters array is 1-indexed)
    1 + Enum.count(1..:counters.info(sieve).size, &(:counters.get(sieve, &1) == 0))
  end
end
