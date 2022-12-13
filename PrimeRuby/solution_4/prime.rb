#!/usr/bin/ruby
# frozen_string_literal: true

require 'concurrent'
require 'numo/narray'

def run_sieve(size, bits)
  factor = 1
  sieve_size_by_2 = (Integer.sqrt(size) / 2)
  bits_view = bits[factor..]
  while factor < sieve_size_by_2
    bits_view.each do |v|
      break if v

      factor += 1
    end
    bits_view = bits[factor + 1..]
    factor2 = 2 * factor
    start = factor2 * (factor + 1) - factor - 1
    step = factor2 + 1
    bits_view[(start..).step(step)] = 0
    factor += 1
  end
  bits
end

SIEVE_SIZE = 1_000_000
pass_count = 0
start_time = Time.now.to_f

reference_results = {
  10 => 4,
  100 => 25,
  1000 => 168,
  10_000 => 1229,
  100_000 => 9592,
  1_000_000 => 78_498,
  10_000_000 => 664_579,
  100_000_000 => 5_761_455
}

# number of cores on your system
cores = 2 #Concurrent.processor_count - 1

workers = (1..cores).map do |i|
  Ractor.new i.to_s do
    loop do
      Ractor.yield run_sieve(SIEVE_SIZE, Numo::Bit.ones((SIEVE_SIZE + 1) / 2))
    end
  end
end

r = nil
begin
  r = Ractor.select(*workers)
  pass_count += 1
end until (Time.now.to_f - start_time) > 5.0

duration = (Time.now.to_f - start_time).round(3)

puts 'WARNING: result is incorrect!' unless reference_results[SIEVE_SIZE] == r[1].count_1
puts "darnellbrawner-MultiThreaded;#{pass_count};#{duration};1;algorithm=base,faithful=no"
