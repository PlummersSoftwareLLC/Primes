#!/usr/bin/ruby

class PrimeSieve
    def initialize(size)
        @sieve_size = size
        @prime_array = Array.new((@sieve_size + 1) / 2, true)
    end

    def count_primes
        @prime_array.count(true)
    end

    def run_sieve
        factor = 3
        sieve_sqrt = Integer.sqrt(@sieve_size)

        while factor <= sieve_sqrt
            number = factor

            until @prime_array[number / 2] || number > sieve_sqrt do number += 2 end

            break unless number <= sieve_sqrt

            factor = number
            number = factor * 3

            while number < @sieve_size
                @prime_array[number / 2] = false
                number += factor * 2
            end

            factor += 2
        end

        @prime_array
    end
end

reference_results = {
    10 => 4,
    100 => 25,
    1000 => 168,
    10000 => 1229,
    100000 => 9592,
    1000000 => 78498,
    10000000 => 664579,
    100000000 => 5761455
}

sieve_size = 1000000
pass_count = 0
start_time = Time.now.to_f
sieve = nil

begin 
    sieve = PrimeSieve.new(sieve_size)
    sieve.run_sieve
    pass_count += 1
end until (Time.now.to_f - start_time) > 5.0

duration = (Time.now.to_f - start_time).round(3)

puts "WARNING: result is incorrect!" unless reference_results[sieve_size] == sieve.count_primes
puts "rbergen;#{ pass_count};#{ duration };1"
