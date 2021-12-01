using Test

include("Primes.jl")

const primeCounts = [
              0 =>         0,
              1 =>         0,
              2 =>         1,
              3 =>         2,
              4 =>         2,
              5 =>         3,
              6 =>         3,
              7 =>         4,
              8 =>         4,
              9 =>         4,
             10 =>         4,
            100 =>        25,
          1_000 =>       168,
         10_000 =>     1_229,
        100_000 =>     9_592,
      1_000_000 =>    78_498,
     10_000_000 =>   664_579,
    100_000_000 => 5_761_455,
]

function isprime(n)
    n ≤ 3 && return n > 1
    (n % 2 == 0 || n % 3 == 0) && return false
    all((n % i != 0 && n % (i + 2) != 0) for i in 5:6:trunc(Int, sqrt(n)))
end

@testset "Sieve of Eratosthenes" begin
    @test Primes(0) |> collect == []
    @test Primes(1) |> collect == []
    @test Primes(2) |> collect == [2]
    @test Primes(3) |> collect == [2, 3]
    @test Primes(4) |> collect == [2, 3]
    @test Primes(5) |> collect == [2, 3, 5]
    @test Primes(6) |> collect == [2, 3, 5]
    @test Primes(7) |> collect == [2, 3, 5, 7]
    @test Primes(8) |> collect == [2, 3, 5, 7]
    @test Primes(9) |> collect == [2, 3, 5, 7]

    @test Primes(0) |> Iterators.reverse |> collect ==           []
    @test Primes(1) |> Iterators.reverse |> collect ==           []
    @test Primes(2) |> Iterators.reverse |> collect ==          [2]
    @test Primes(3) |> Iterators.reverse |> collect ==       [3, 2]
    @test Primes(4) |> Iterators.reverse |> collect ==       [3, 2]
    @test Primes(5) |> Iterators.reverse |> collect ==    [5, 3, 2]
    @test Primes(6) |> Iterators.reverse |> collect ==    [5, 3, 2]
    @test Primes(7) |> Iterators.reverse |> collect == [7, 5, 3, 2]
    @test Primes(8) |> Iterators.reverse |> collect == [7, 5, 3, 2]
    @test Primes(9) |> Iterators.reverse |> collect == [7, 5, 3, 2]

    for (limit, n) ∈ primeCounts
        primes = Primes(limit)
        @test length(primes) === n
        @test all(isprime(p) for p in primes)
    end
end
