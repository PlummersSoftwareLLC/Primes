# frozen_string_literal: true

require 'numo/narray'

class PrimeSieve

  def initialize(limit)
    @size = limit
    @bits = Numo::Bit.ones((@size + 1) / 2)
  end

  def run_sieve
    factor = 1
    sieve_size_by_2 = (Integer.sqrt(@size) / 2)
    bits_view = @bits[factor..]
    while factor < sieve_size_by_2
      bits_view.each do |v|
        break if v

        factor += 1
      end
      bits_view = @bits[factor + 1..]
      factor2 = 2 * factor
      start = factor2 * (factor + 1) - factor - 1
      step = factor2 + 1
      bits_view[(start..).step(step)] = 0
      factor += 1
    end
  end

  def count_primes
    @bits.count_1
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
  sieve = PrimeSieve.new(sieve_size)
  sieve.run_sieve
  pass_count += 1
  break if (Time.now.to_f - start_time) > 5.0
end
duration = (Time.now.to_f - start_time).round(3)

puts "WARNING: result is incorrect! #{reference_results[sieve_size]} != #{sieve.count_primes}" unless reference_results[sieve_size] == sieve.count_primes
puts "darnellbrawner;#{pass_count};#{duration};1;algorithm=base,faithful=no"
