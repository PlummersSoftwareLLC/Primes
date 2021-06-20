require "bit_array"

DICT = {
           10_u64 => 4,
          100_u64 => 25,
         1000_u64 => 168,
        10000_u64 => 1229,
       100000_u64 => 9592,
      1000000_u64 => 78498,
     10000000_u64 => 664579,
    100000000_u64 => 5761455,
   1000000000_u64 => 50847534,
  10000000000_u64 => 455052511,
}

struct PrimeSieve
  def initialize(@sieve_size : UInt64)
    @bits = BitArray.new(@sieve_size.to_i32)
  end

  def run_sieve
    factor = 3_u64
    q = Math.sqrt(@sieve_size).to_u64

    while factor <= q
      num = factor

      while num < @sieve_size
        if !@bits[num]
          factor = num
          break
        end
        num += 2_u64
      end

      num2 = factor * factor
      while num2 < @sieve_size
        @bits[num2] = true
        num2 += factor * 2_u64
      end

      factor += 2_u64
    end
  end

  def print_results(show_results : Bool, duration : Float64, passes : Int32)
    if show_results
      printf("2, ")
    end

    count = 1
    (3..@sieve_size).step(2).each do |v|
      if !@bits[v]
        printf("%d", v) if show_results
        count += 1
      end
    end

    if show_results
      puts
    end

    printf("Passes: %d Time: %f Avg: %f Limit: %d Count1: %d Count2: %d Valid: %s\n",
      passes, duration, (duration / passes), @sieve_size, count,
      count_primes(), validate_results(),
    )

    printf("marghidanu;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, duration)
  end

  def count_primes
    count = (3_u64..@sieve_size).step(2_u64).select { |v| !@bits[v] }
    count.size + 1
  end

  def validate_results
    DICT[@sieve_size] == count_primes()
  end
end

passes = 0
start_time = Time.monotonic

loop do
  sieve = PrimeSieve.new(1000000_u64)
  sieve.run_sieve

  passes += 1
  duration = Time.monotonic - start_time
  if duration.total_seconds >= 5_f64
    sieve.print_results(false, duration.total_seconds, passes)
    break
  end
end
