enum SIEVE_SIZE, NUM_BITS, SIEVE
constant STDOUT = 1
constant TRUE = 1
constant FALSE = 0

without type_check
with inline

-- Phix have bit shift operations, but they are slow, so need to have a bit table for speed --
sequence BITS_TABLE = {
    0x0000_0001,
    0x0000_0002,
    0x0000_0004,
    0x0000_0008,
    0x0000_0010,
    0x0000_0020,
    0x0000_0040,
    0x0000_0080,
    0x0000_0100,
    0x0000_0200,
    0x0000_0400,
    0x0000_0800,
    0x0000_1000,
    0x0000_2000,
    0x0000_4000,
    0x0000_8000,
    0x0001_0000,
    0x0002_0000,
    0x0004_0000,
    0x0008_0000,
    0x0010_0000,
    0x0020_0000,
    0x0040_0000,
    0x0080_0000,
    0x0100_0000,
    0x0200_0000,
    0x0400_0000,
    0x0800_0000,
    0x1000_0000,
    0x2000_0000,
    0x4000_0000,
    0x8000_0000
}
sequence NOT_BITS_TABLE = {
    0xFFFF_FFFE,
    0xFFFF_FFFD,
    0xFFFF_FFFB,
    0xFFFF_FFF7,
    0xFFFF_FFEF,
    0xFFFF_FFDF,
    0xFFFF_FFBF,
    0xFFFF_FF7F,
    0xFFFF_FEFF,
    0xFFFF_FDFF,
    0xFFFF_FBFF,
    0xFFFF_F7FF,
    0xFFFF_EFFF,
    0xFFFF_DFFF,
    0xFFFF_BFFF,
    0xFFFF_7FFF,
    0xFFFE_FFFF,
    0xFFFD_FFFF,
    0xFFFB_FFFF,
    0xFFF7_FFFF,
    0xFFEF_FFFF,
    0xFFDF_FFFF,
    0xFFBF_FFFF,
    0xFF7F_FFFF,
    0xFEFF_FFFF,
    0xFDFF_FFFF,
    0xFBFF_FFFF,
    0xF7FF_FFFF,
    0xEFFF_FFFF,
    0xDFFF_FFFF,
    0xBFFF_FFFF,
    0x7FFF_FFFF
}

function set_bits(integer num_bits)
    integer num_words = floor((num_bits + 31) / 32)
    integer num_leftover_bits = 1 + and_bits(num_bits - 1, 0x1f)
    integer all_ones = not_bits(0)
    integer leftover_bits = BITS_TABLE[num_leftover_bits] * 2 - 1
    return repeat(all_ones, num_words - 1) & leftover_bits
end function

function clear_bits(sequence this, integer num_bits, integer start, integer step)
    integer word_num = 1 + floor((start - 1) / 32)
    integer bit_pos = 1 + and_bits(start - 1, 0x1f)
    integer word_inc = floor(step / 32)
    integer bit_inc = and_bits(step, 0x1f)
    for k = start to num_bits by step
    do
        this[word_num] = and_bits(this[word_num], NOT_BITS_TABLE[bit_pos])
        word_num += word_inc
        bit_pos += bit_inc
        if bit_pos > 32
        then
            bit_pos -= 32
            word_num += 1
        end if
    end for
    return this
end function

function get_bit(sequence this, integer bit_num)
    integer word_pos = 1 + floor((bit_num - 1) / 32)
    integer bit_pos = 1 + and_bits(bit_num - 1, 0x1f)
    return and_bits(this[word_pos], BITS_TABLE[bit_pos])
end function

function run_sieve(integer sieve_size)
    integer num_bits = floor((sieve_size - 1) / 2)
    sequence sieve = set_bits(num_bits)
    integer q = floor(sqrt(sieve_size) / 2)
    for bit = 1 to q
    do
        if get_bit(sieve, bit)
        then
            sieve = clear_bits(sieve, num_bits, 2 * bit * (bit + 1), 2 * bit + 1)
        end if
    end for

    return {sieve_size, num_bits, sieve}
end function

function count_primes(sequence this, integer show_results=FALSE)
    integer count = 1
    if show_results = TRUE
    then
        printf(STDOUT, "2, ")
    end if

    for bit = 1 to this[NUM_BITS]
    do
        if get_bit(this[SIEVE], bit)
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
    -- Cannot support sieve size greater than 1 billion --
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
        case 100_000_000 then expected_count = 5761455
        case 1_000_000_000 then expected_count = 50847534
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
            iff(validate_results(this), "true", "false")
        }
    )
    printf(
        STDOUT,
        "\nrzuckerm;%d;%.8f;1;algorithm=base,faithful=yes,bits=1\n",
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

