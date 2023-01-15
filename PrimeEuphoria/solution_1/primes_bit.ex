include std/io.e
without type_check

enum SIEVE_SIZE, NUM_BITS, SIEVE
constant TRUE = 1
constant FALSE = 0

-- Euphoria does not have bit shift operations, so need to have a bit table for speed --
sequence BITS_TABLE = {
    0x0000_0000_0000_0001,
    0x0000_0000_0000_0002,
    0x0000_0000_0000_0004,
    0x0000_0000_0000_0008,
    0x0000_0000_0000_0010,
    0x0000_0000_0000_0020,
    0x0000_0000_0000_0040,
    0x0000_0000_0000_0080,
    0x0000_0000_0000_0100,
    0x0000_0000_0000_0200,
    0x0000_0000_0000_0400,
    0x0000_0000_0000_0800,
    0x0000_0000_0000_1000,
    0x0000_0000_0000_2000,
    0x0000_0000_0000_4000,
    0x0000_0000_0000_8000,
    0x0000_0000_0001_0000,
    0x0000_0000_0002_0000,
    0x0000_0000_0004_0000,
    0x0000_0000_0008_0000,
    0x0000_0000_0010_0000,
    0x0000_0000_0020_0000,
    0x0000_0000_0040_0000,
    0x0000_0000_0080_0000,
    0x0000_0000_0100_0000,
    0x0000_0000_0200_0000,
    0x0000_0000_0400_0000,
    0x0000_0000_0800_0000,
    0x0000_0000_1000_0000,
    0x0000_0000_2000_0000,
    0x0000_0000_4000_0000,
    0x0000_0000_8000_0000,
    0x0000_0001_0000_0000,
    0x0000_0002_0000_0000,
    0x0000_0004_0000_0000,
    0x0000_0008_0000_0000,
    0x0000_0010_0000_0000,
    0x0000_0020_0000_0000,
    0x0000_0040_0000_0000,
    0x0000_0080_0000_0000,
    0x0000_0100_0000_0000,
    0x0000_0200_0000_0000,
    0x0000_0400_0000_0000,
    0x0000_0800_0000_0000,
    0x0000_1000_0000_0000,
    0x0000_2000_0000_0000,
    0x0000_4000_0000_0000,
    0x0000_8000_0000_0000,
    0x0001_0000_0000_0000,
    0x0002_0000_0000_0000,
    0x0004_0000_0000_0000,
    0x0008_0000_0000_0000,
    0x0010_0000_0000_0000,
    0x0020_0000_0000_0000,
    0x0040_0000_0000_0000,
    0x0080_0000_0000_0000,
    0x0100_0000_0000_0000,
    0x0200_0000_0000_0000,
    0x0400_0000_0000_0000,
    0x0800_0000_0000_0000,
    0x1000_0000_0000_0000,
    0x2000_0000_0000_0000,
    0x4000_0000_0000_0000,
    0x8000_0000_0000_0000
}
sequence NOT_BITS_TABLE = {
    0xFFFF_FFFF_FFFF_FFFE,
    0xFFFF_FFFF_FFFF_FFFD,
    0xFFFF_FFFF_FFFF_FFFB,
    0xFFFF_FFFF_FFFF_FFF7,
    0xFFFF_FFFF_FFFF_FFEF,
    0xFFFF_FFFF_FFFF_FFDF,
    0xFFFF_FFFF_FFFF_FFBF,
    0xFFFF_FFFF_FFFF_FF7F,
    0xFFFF_FFFF_FFFF_FEFF,
    0xFFFF_FFFF_FFFF_FDFF,
    0xFFFF_FFFF_FFFF_FBFF,
    0xFFFF_FFFF_FFFF_F7FF,
    0xFFFF_FFFF_FFFF_EFFF,
    0xFFFF_FFFF_FFFF_DFFF,
    0xFFFF_FFFF_FFFF_BFFF,
    0xFFFF_FFFF_FFFF_7FFF,
    0xFFFF_FFFF_FFFE_FFFF,
    0xFFFF_FFFF_FFFD_FFFF,
    0xFFFF_FFFF_FFFB_FFFF,
    0xFFFF_FFFF_FFF7_FFFF,
    0xFFFF_FFFF_FFEF_FFFF,
    0xFFFF_FFFF_FFDF_FFFF,
    0xFFFF_FFFF_FFBF_FFFF,
    0xFFFF_FFFF_FF7F_FFFF,
    0xFFFF_FFFF_FEFF_FFFF,
    0xFFFF_FFFF_FDFF_FFFF,
    0xFFFF_FFFF_FBFF_FFFF,
    0xFFFF_FFFF_F7FF_FFFF,
    0xFFFF_FFFF_EFFF_FFFF,
    0xFFFF_FFFF_DFFF_FFFF,
    0xFFFF_FFFF_BFFF_FFFF,
    0xFFFF_FFFF_7FFF_FFFF,
    0xFFFF_FFFE_FFFF_FFFF,
    0xFFFF_FFFD_FFFF_FFFF,
    0xFFFF_FFFB_FFFF_FFFF,
    0xFFFF_FFF7_FFFF_FFFF,
    0xFFFF_FFEF_FFFF_FFFF,
    0xFFFF_FFDF_FFFF_FFFF,
    0xFFFF_FFBF_FFFF_FFFF,
    0xFFFF_FF7F_FFFF_FFFF,
    0xFFFF_FEFF_FFFF_FFFF,
    0xFFFF_FDFF_FFFF_FFFF,
    0xFFFF_FBFF_FFFF_FFFF,
    0xFFFF_F7FF_FFFF_FFFF,
    0xFFFF_EFFF_FFFF_FFFF,
    0xFFFF_DFFF_FFFF_FFFF,
    0xFFFF_BFFF_FFFF_FFFF,
    0xFFFF_7FFF_FFFF_FFFF,
    0xFFFE_FFFF_FFFF_FFFF,
    0xFFFD_FFFF_FFFF_FFFF,
    0xFFFB_FFFF_FFFF_FFFF,
    0xFFF7_FFFF_FFFF_FFFF,
    0xFFEF_FFFF_FFFF_FFFF,
    0xFFDF_FFFF_FFFF_FFFF,
    0xFFBF_FFFF_FFFF_FFFF,
    0xFF7F_FFFF_FFFF_FFFF,
    0xFEFF_FFFF_FFFF_FFFF,
    0xFDFF_FFFF_FFFF_FFFF,
    0xFBFF_FFFF_FFFF_FFFF,
    0xF7FF_FFFF_FFFF_FFFF,
    0xEFFF_FFFF_FFFF_FFFF,
    0xDFFF_FFFF_FFFF_FFFF,
    0xBFFF_FFFF_FFFF_FFFF,
    0x7FFF_FFFF_FFFF_FFFF
}

function set_bits(integer num_bits)
    integer num_words = floor((num_bits + 63) / 64)
    atom all_ones = not_bits(0)
    atom leftover_bits = power(2, and_bits(num_bits, 0x3f)) - 1
    if leftover_bits = 0
    then
        leftover_bits = all_ones
    end if

    return repeat(all_ones, num_words - 1) & leftover_bits
end function

function clear_bits(sequence this, integer num_bits, integer start, integer step)
    integer word_num = 1 + floor((start - 1) / 64)
    integer bit_pos = 1 + and_bits((start - 1), 0x3f)
    integer word_inc = floor(step / 64)
    integer bit_inc = and_bits(step, 0x3f)
    for k = start to num_bits by step
    do
        this[word_num] = and_bits(this[word_num], NOT_BITS_TABLE[bit_pos])
        word_num += word_inc
        bit_pos += bit_inc
        if bit_pos > 64
        then
            bit_pos -= 64
            word_num += 1
        end if
    end for
    return this
end function

function get_bit(sequence this, integer bit_num)
    integer word_pos = 1 + floor((bit_num - 1) / 64)
    integer bit_pos = 1 + and_bits(bit_num - 1, 0x3f)
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

function count_primes(sequence this)
    integer count = 1
    for bit = 1 to this[NUM_BITS]
    do
        if get_bit(this[SIEVE], bit)
        then
            count += 1
        end if
    end for

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

procedure print_results(sequence this, atom show_results, atom duration, integer passes)
    if show_results = TRUE
    then
        printf(STDOUT, "2, ")
    end if

    integer count = 1
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

    atom valid = "false"
    if validate_results(this)
    then
        valid = "true"
    end if

    -- Euphoria seems to only provide time in 1/100th of a second --
    printf(
        STDOUT,
        "Passes: %d, Time: %.2f, Avg: %.8f, Limit: %d, Count1: %d, Count2: %d, Valid: %s\n",
        {
            passes,
            duration,
            duration / passes,
            this[SIEVE_SIZE],
            count,
            count_primes(this),
            valid
        }
    )
    printf(
        STDOUT,
        "\nrzuckerm;%d;%.2f;1;algorithm=base,faithful=yes,bits=1\n",
        {passes, duration}
    )
end procedure

procedure main()
    atom start = time()
    integer n = 1_000_000
    integer passes = 0
    atom show_results = FALSE
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
