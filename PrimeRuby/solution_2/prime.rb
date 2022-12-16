# frozen_string_literal: true
#
require 'concurrent'
require 'numo/narray'

class PrimeSieve

  def initialize(limit, arr)
    @size = limit
    @prime_array = arr
  end

  def run_sieve_numo
    factor = 1
    sieve_size_by_2 = (Integer.sqrt(@size) / 2)
    bits_view = @prime_array[factor..]
    while factor < sieve_size_by_2
      bits_view.each do |v|
        break if v

        factor += 1
      end
      bits_view = @prime_array[factor + 1..]
      factor2 = 2 * factor
      start = factor2 * (factor + 1) - factor - 1
      step = factor2 + 1
      bits_view[(start..).step(step)] = 0
      factor += 1
    end
    @prime_array
  end

  def run_sieve()
    factor = 3
    sieve_sqrt = Integer.sqrt(@size)
    sqrt_by_2_plus_one = sieve_sqrt / 2 + 1
    sieve_size_by_2 = @size / 2

    while factor <= sieve_sqrt
      number = factor
      div_2 = number / 2
      div_2 += 1 until @prime_array[div_2] || div_2 > sqrt_by_2_plus_one
      number = div_2 * 2 + 1
      break unless number <= sieve_sqrt

      factor = number
      number = factor * factor / 2
      while number < sieve_size_by_2
        @prime_array[number] = false
        number += factor
      end
      factor += 2
    end
    @prime_array
  end

  def count_primes
    (@prime_array.class == Numo::Bit) ? @prime_array.count_1 : @prime_array.count(true)
  end
end

reference_results = { 10 => 4,
                      100 => 25,
                      1000 => 168,
                      10_000 => 1229,
                      100_000 => 9592,
                      1_000_000 => 78_498,
                      10_000_000 => 664_579,
                      100_000_000 => 5_761_455 }.freeze
sieve_size = 1_000_000
pass_count = 0
start_time = Time.now.to_f
sieve = nil

loop do
  sieve = PrimeSieve.new(sieve_size, Numo::Bit.ones((sieve_size + 1) / 2))
  sieve.run_sieve_numo
  pass_count += 1
  break if (Time.now.to_f - start_time) > 5.0
end
duration = (Time.now.to_f - start_time).round(3)

puts "WARNING: result is incorrect! #{reference_results[sieve_size]} != #{sieve.count_primes}" unless reference_results[sieve_size] == sieve.count_primes
puts "darnellbrawner-Numo;#{pass_count};#{duration};1;algorithm=base,faithful=no"

cores = Concurrent.processor_count
pass_count = 0
start_time = Time.now.to_f

workers = (1..cores).map do |i|
  sieve = PrimeSieve.new(sieve_size, Array.new((sieve_size + 1) / 2, true))
  Ractor.new (sieve) do |sieve|
    loop do
      Ractor.yield sieve.run_sieve
    end
  end
end

r = nil
begin
  r = Ractor.select(*workers)
  pass_count += 1
end until (Time.now.to_f - start_time) > 5.0
duration = (Time.now.to_f - start_time).round(3)

puts 'WARNING: result is incorrect!' unless reference_results[sieve_size] == r[1].count(true)
puts "darnellbrawner-MultiThreaded;#{pass_count};#{duration};#{cores};algorithm=base,faithful=yes"

cores = 2 #Concurrent.processor_count - 1
pass_count = 0
start_time = Time.now.to_f

workers = (1..cores).map do |i|
  sieve = PrimeSieve.new(sieve_size, Numo::Bit.ones((sieve_size + 1) / 2))
  Ractor.new(sieve) do |sieve|
    loop do
      Ractor.yield sieve.run_sieve_numo
    end
  end
end

r = nil
begin
  r = Ractor.select(*workers)
  pass_count += 1
end until (Time.now.to_f - start_time) > 5.0
duration = (Time.now.to_f - start_time).round(3)

puts 'WARNING: result is incorrect!' unless reference_results[sieve_size] == r[1].count_1
puts "darnellbrawner-MultiThreaded-Numo_2core;#{pass_count};#{duration};#{cores};algorithm=base,faithful=no"