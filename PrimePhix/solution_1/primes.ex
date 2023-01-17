enum SIEVE_SIZE, SIEVE
constant STDOUT = 1
constant TRUE = 1
constant FALSE = 0

without type_check

function run_sieve(integer sieve_size)
    integer num_bits = floor((sieve_size - 1) / 2)
    sequence sieve = repeat(TRUE, num_bits)
    integer q = floor(sqrt(sieve_size) / 2)
    for bit = 1 to q
    do
        if sieve[bit] = TRUE
        then
            for k = 2 * bit * (bit + 1) to num_bits by 2 * bit + 1
            do
                sieve[k] = FALSE
            end for
        end if
    end for

    return {sieve_size, sieve}
end function

function count_primes(sequence this, integer show_results=FALSE)
    integer count = 1
    if show_results = TRUE
    then
        printf(STDOUT, "2, ")
    end if
    
    for bit = 1 to length(this[SIEVE])
    do
        if this[SIEVE][bit] = TRUE
        then
            count += 1
            if show_results = TRUE
            then
                printf(STDOUT, "%d, ", 2 * bit + 1)
            end if
        end if
    end for

    if show_results = TRUE
    then
        printf(STDOUT, "\n")
    end if

    return count
end function

function validate_results(sequence this)
    -- Cannot support sieve size greater than 10 million --
    integer expected_count = -1
    switch this[SIEVE_SIZE]
    do
        case 10 then expected_count = 4
        case 100 then expected_count = 25
        case 1_000 then expected_count = 168
        case 10_000 then expected_count = 1229
        case 100_000 then expected_count = 9592
        case 1_000_000 then expected_count = 78498
        case 10_000_000 then expected_count = 664579
        case else printf(STDOUT, "Invalid sieve size\n")
    end switch

    return count_primes(this) = expected_count
end function

procedure print_results(sequence this, integer show_results, atom duration, integer passes)
    printf(
        STDOUT,
        "Passes: %d, Time: %.8f, Avg: %.8f, Limit: %d, Count: %d, Valid: %s\n",
        {
            passes,
            duration,
            duration / passes,
            this[SIEVE_SIZE],
            count_primes(this, show_results),
            iff(validate_results(this) ? "true" : "false")
        }
    )
    printf(
        STDOUT,
        "\nrzuckerm;%d;%.8f;1;algorithm=base,faithful=yes\n",
        {passes, duration}
    )
end procedure

procedure main()
    atom start = time()
    integer n = 1_000_000
    integer passes = 0
    integer show_results = FALSE
    sequence sieve
    atom duration
    while 1 do
        passes += 1
        sieve = run_sieve(n)
        duration = time() - start
        if duration >= 5
        then
            print_results(sieve, show_results, duration, passes)
            exit
        end if
    end while
end procedure

main()
