include std/io.e
include std/utils.e
include std/primes.e
without type_check

constant TRUE = 1
constant FALSE = 0

function count_primes(sequence primes, integer show_results=FALSE)
    integer n = length(primes)
    if show_results = TRUE
    then
        for k = 1 to n
        do
            printf(STDOUT, "%d, ", primes[k])
        end for
        printf(STDOUT, "\n")
    end if

    return n
end function

function validate_results(sequence primes, integer sieve_size)
    -- Cannot support sieve size greater than 1 million --
    integer expected_count = -1
    switch sieve_size
    do
        case 10 then expected_count = 4
        case 100 then expected_count = 25
        case 1_000 then expected_count = 168
        case 10_000 then expected_count = 1229
        case 100_000 then expected_count = 9592
        case 1_000_000 then expected_count = 78498
        case else printf(STDOUT, "Invalid sieve size\n")
    end switch

    return count_primes(primes) = expected_count
end function

procedure print_results(sequence primes, integer sieve_size, integer show_results, atom duration, integer passes)
    -- Euphoria seems to only provide time in 1/100th of a second --
    printf(
        STDOUT,
        "Passes: %d, Time: %.2f, Avg: %.8f, Limit: %d, Count: %d, Valid: %s\n",
        {
            passes,
            duration,
            duration / passes,
            sieve_size,
            count_primes(primes, show_results),
            iff(validate_results(primes, sieve_size), "true", "false")
        }
    )
    printf(
        STDOUT,
        "\nrzuckerm-builtin;%d;%.2f;1;algorithm=other,faithful=no\n",
        {passes, duration}
    )
end procedure

procedure main()
    atom start = time()
    integer n = 1_000_000
    integer passes = 0
    integer show_results = FALSE
    sequence primes
    atom duration
    while 1 do
        passes += 1
        primes = prime_list(n)
        duration = time() - start
        if duration >= 5.0
        then
            print_results(primes, n, show_results, duration, passes)
            exit
        end if
    end while
end procedure

main()
