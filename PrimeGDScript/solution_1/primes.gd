extends SceneTree

class prime_sieve:

    # Historical data for validating our results - the number of primes
    # to be found under some limit, such as 168 primes under 1000  

    var results_dictionary := {
        10: 4,
        100: 25,
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455,
        1000000000: 508475345,
        10000000000: 455052511
    }

    var sieve_size: int
    var bits: PoolByteArray

    func _init(n: int):
        sieve_size = n
        bits.resize(sieve_size)


    func run_sieve() -> void:
        var factor := 3
        var q := sqrt(sieve_size) as int

        while (factor <= q):
            for num in range(factor, sieve_size, 2):
                if bits[num] == 0:
                    factor = num
                    break
            for num in range(factor * factor, sieve_size, factor * 2):
                bits[num] = 1
        
            factor += 2
    

    func print_results(show_results: bool, duration: float, passes: int) -> void:
            if show_results:
                printraw("2, ")
            
            var count := 1
            for num in range(3, sieve_size, 2):
                if bits[num] == 0:
                    if (show_results):
                        printraw("%s, " % num)
                    count += 1

            print("\nPasses: %s, Time: %sms, Avg: %s, Limit: %s, Count1: %s, Count2: %s, Valid: %s" %
                [passes,
                duration,
                duration / passes,
                sieve_size,
                count,
                count_primes(),
                _validate_results()])
            print("OrigamiDev-Pete;%s;%s;1;algorithm=base,faithful=yes,bits=8" %
                [passes,
                duration/1000])
    

    func count_primes() -> int:
        var count = 1
        for i in range(3, sieve_size, 2):
            if bits[i] == 0:
                count += 1
        return count


    func _validate_results() -> bool:
        if results_dictionary.has(sieve_size):
            return results_dictionary[sieve_size] == count_primes()
        return false
        

func _init():

    var passes := 0
    var time_start := OS.get_ticks_msec()

    while true:
        var sieve = prime_sieve.new(1000000)
        sieve.run_sieve()
        passes += 1

        if OS.get_ticks_msec() - time_start >= 5000:
            sieve.print_results(false, OS.get_ticks_msec() - time_start / 1000, passes)
            break

    quit()
