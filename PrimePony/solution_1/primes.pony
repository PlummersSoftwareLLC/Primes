use "collections"
use "format"
use "time"

class PrimeSieve
    let _env: Env
    let _dictionary: Map[U64, U64]
    let _sieve_size: U64
    let _bits: Array[Bool]

    new create(env': Env, dictionary': Map[U64, U64], sieve_size': U64) =>
        _env = env'
        _dictionary = dictionary'

        _sieve_size = sieve_size'
        _bits = Array[Bool].init(false, _sieve_size.usize())

    fun ref run_sieve() =>
        var factor : U64 = U64(3)
        let q = _sieve_size.f64().sqrt().u64()

        while factor <= q do
            var num = factor
            while num < _sieve_size do
                try 
                   if not _bits(num.usize())? then
                        factor = num
                        break
                    end
                end

                num = num + 2
            end

            var num2 = factor * factor
            while num2  < _sieve_size do
                try _bits.update(num2.usize(), true)? end
                num2 = num2 + (factor * 2)
            end

            factor = factor + 2
        end

    fun print_results(show_results: Bool, duration: I64, passes: U64) =>
        if show_results then
            _env.out.print("2")
        end

        var count: U64 = 1
        for index in Range[U64](3, _sieve_size, 2) do
            try 
                if not _bits(index.usize())? then
                    if show_results then _env.out.print(Format.int[U64](index)) end
                    count = count + 1
                end
            end
        end

        if show_results then
            _env.out.print("")
        end

        _env.out.print(
            "Passes: " + Format.int[U64](passes) + " " +
            "Time: " + Format.int[I64](duration) + " " +
            "Limit: " + Format.int[U64](_sieve_size) + " " +
            "Count1: " + Format.int[U64](count) + " " +
            "Count2: " + Format.int[U64](count_primes()) + " " +
            "Valid: " + validate_results().string()
        )

        _env.out.print(
            "marghidanu;" +
            Format.int[U64](passes) + ";" +
            Format.int[I64](duration) + ";" +
            "1;algorithm=base,faithful=yes,bits=1"
        )

    fun count_primes() : U64 =>
        var count : U64 = 1
        for index in Range(3, _sieve_size.usize(), 2) do
            try 
                if not _bits(index)? then
                    count = count + 1
                end
            end
        end

        count

    fun validate_results() : Bool => 
        _dictionary.get_or_else(_sieve_size, 0) == count_primes()


actor Main
    new create(env: Env) => 
        var passes = U64(0)
        let start_time = Time.now()._1

        let dictionary = Map[U64, U64].create(10)
        dictionary.insert(10, 4)
        dictionary.insert(100, 25)
        dictionary.insert(1_000, 168)
        dictionary.insert(10_000, 1229)
        dictionary.insert(100_000, 9592)
        dictionary.insert(1_000_000, 78498)
        dictionary.insert(10_000_000, 664579)
        dictionary.insert(100_000_000, 5761455)
        dictionary.insert(1_000_000_000, 50847534)
        dictionary.insert(10_000_000_000, 45505251)

        while true do
            // NOTE: Pony doesn't allow global variables
            // therefor we need to pass all dependencies to the class instance.
            let sieve = PrimeSieve.create(env, dictionary, U64(1_000_000))
            sieve.run_sieve()
            passes = passes + 1

            // Unfortunately I can only get the seconds as an integer value.
            // Pony seems fast enough to make the time variance minimal.
            let duration = Time.now()._1 - start_time
            if duration >= 5 then 
                sieve.print_results(false, duration, passes)
                break
            end
        end
