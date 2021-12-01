defmodule PrimeSieveTest do
  @expected %{
    10 => 4,
    100 => 25,
    1000 => 168,
    10000 => 1229,
    100_000 => 9592,
    1_000_000 => 78498
  }

  use ExUnit.Case
  doctest PrimeSieve

  test "next_factor" do
    assert 5 == PrimeSieve.next_factor(<<0b0011::size(4)>>, 1)
    assert 5 == PrimeSieve.next_factor(<<0b0011::size(4)>>, 3)
    assert 5 == PrimeSieve.next_factor(<<0b0011::size(4)>>, 5)
    assert 7 == PrimeSieve.next_factor(<<0b0011::size(4)>>, 6)
  end

  test "get_bit small" do
    # 0000010
    t = <<0b001::size(3)>>
    assert 0 == PrimeSieve.get_bit(t, 0)
    assert 0 == PrimeSieve.get_bit(t, 1)
    assert 0 == PrimeSieve.get_bit(t, 2)
    assert 0 == PrimeSieve.get_bit(t, 3)
    assert 0 == PrimeSieve.get_bit(t, 4)
    assert 1 == PrimeSieve.get_bit(t, 5)
    assert 0 == PrimeSieve.get_bit(t, 6)
  end

  test "get_bit medium" do
    # 00010100000101000001010000010100
    t = <<0b0110011001100110::size(16)>>

    assert [
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0
           ] == Enum.map(0..32, fn x -> PrimeSieve.get_bit(t, x) end)
  end

  test "print_sieve" do
    t = <<0b0110011001100110::size(16)>>

    assert [
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0,
             0,
             0,
             0,
             1,
             0,
             1,
             0,
             0
           ] == PrimeSieve.print_sieve(t)
  end

  test "print_primes" do
    assert [
             2,
             3,
             5,
             7,
             11,
             13,
             17,
             19,
             23,
             29,
             31,
             37,
             41,
             43,
             47,
             53,
             59,
             61,
             67,
             71,
             73,
             79,
             83,
             89,
             97
           ] ==
             100
             |> PrimeSieve.create_prime_sieve()
             |> PrimeSieve.print_primes()
  end

  test "create new sieve" do
    assert assert <<0b111111::size(6)>> = PrimeSieve.create_sieve(12)
  end

  test "count_primes" do
    assert 6 == PrimeSieve.count_primes(<<0b111111::size(6)>>)
    assert 5 == PrimeSieve.count_primes(<<0b101111::size(6)>>)
  end

  test "count_primes skips first one, but counts 2 as a prime" do
    assert 1 == PrimeSieve.count_primes(<<0b1::size(1)>>)
  end

  test "clear_bits" do
    assert 6 = PrimeSieve.clear_bits(<<-1::size(6)>>, 3) |> bit_size()
    assert <<0b111101::size(6)>> = PrimeSieve.clear_bits(<<-1::size(6)>>, 3)
  end

  test "part" do
    sieve = PrimeSieve.create_sieve(12)
    sieve = PrimeSieve.clear_bit(sieve, 3)
    assert <<0b101111::size(6)>> == sieve
    sieve = PrimeSieve.clear_bit(sieve, 7)
    assert <<0b101011::size(6)>> == sieve

    assert 1 == PrimeSieve.get_bit(sieve, 1)
    assert 0 == PrimeSieve.get_bit(sieve, 3)
    assert 0 == PrimeSieve.get_bit(sieve, 4)
    assert 1 == PrimeSieve.get_bit(sieve, 5)
    assert 0 == PrimeSieve.get_bit(sieve, 7)
  end

  Enum.map(@expected, fn {amount, expected} ->
    @amount amount
    @expected_result expected
    test "run #{@amount}" do
      assert @expected_result == PrimeSieve.run(@amount)
    end
  end)
end
