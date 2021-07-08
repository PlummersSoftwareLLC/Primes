defmodule PrimeSieveTest do
  use ExUnit.Case

  test "all the primes" do
    Enum.each(
    [10,
     100,
     1000,
     10_000,
     100_000,
     1_000_000,
     10_000_000,
     100_000_000], fn size ->
       :timer.send_after(200, self(), :stop)
       PrimeSieve.run(size)
     end)
  end
end
