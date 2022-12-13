#!/usr/bin/ruby
# frozen_string_literal: true

require 'concurrent'
def run_sieve(sieve_size, prime_array)
  factor = 3
  sieve_sqrt = Integer.sqrt(sieve_size)
  sqrt_by_2_plus_one = sieve_sqrt / 2 + 1
  sieve_size_by_2 = sieve_size / 2

  while factor <= sieve_sqrt
    number = factor
    div_2 = number / 2
    div_2 += 1 until prime_array[div_2] || div_2 > sqrt_by_2_plus_one
    number = div_2 * 2 + 1
    break unless number <= sieve_sqrt

    factor = number
    number = factor * factor / 2
    while number < sieve_size_by_2
      prime_array[number] = false
      number += factor
    end
    factor += 2
  end
  prime_array
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
cores = Concurrent.processor_count - 1

pipe = Ractor.new 'listen_and_send' do
  loop do
    Ractor.yield Ractor.receive
  end
end

workers = (1..cores).map do |i|
  Ractor.new i.to_s do
    loop do
      Ractor.yield run_sieve(SIEVE_SIZE, Array.new((SIEVE_SIZE + 1) / 2, true))
    end
  end
end

r = nil
loop do
  pipe << pass_count
  r = Ractor.select(*workers)
  pass_count += 1
  break if (Time.now.to_f - start_time) > 5.0
end

duration = (Time.now.to_f - start_time).round(3)

puts 'WARNING: result is incorrect!' unless reference_results[SIEVE_SIZE] == r[1].count(true)
puts "darnellbrawner;#{pass_count};#{duration};1;algorithm=base,faithful=no"
