HAI 1.4
CAN HAS STRING?
CAN HAS CLOCK?

I HAS A SIEVE_SIZE ITZ 1000000
I HAS A EXPECTED_COUNT ITZ 78498
I HAS A TARGET_USEC ITZ 5000000
BTW POP8 is a process-wide lookup table, not per-pass sieve state.
I HAS A POP8 ITZ A BUKKIT

I HAS A pop_idx ITZ 0
IM IN YR init_pop8
    BOTH SAEM pop_idx AN 256, O RLY?
        YA RLY
            GTFO
    OIC
    I HAS A pop_value ITZ pop_idx
    I HAS A pop_count ITZ 0
    IM IN YR count_pop_bits
        BOTH SAEM pop_value AN 0, O RLY?
            YA RLY
                GTFO
        OIC
        pop_count R SUM OF pop_count AN MOD OF pop_value AN 2
        pop_value R QUOSHUNT OF pop_value AN 2
    IM OUTTA YR count_pop_bits
    POP8 HAS A SRS pop_idx ITZ pop_count
    pop_idx R SUM OF pop_idx AN 1
IM OUTTA YR init_pop8

HOW IZ I select_mask YR bit_offset
    bit_offset, WTF?
        OMG 0, FOUND YR 1
        OMG 1, FOUND YR 2
        OMG 2, FOUND YR 4
        OMG 3, FOUND YR 8
        OMG 4, FOUND YR 16
        OMG 5, FOUND YR 32
        OMG 6, FOUND YR 64
        OMG 7, FOUND YR 128
        OMG 8, FOUND YR 256
        OMG 9, FOUND YR 512
        OMG 10, FOUND YR 1024
        OMG 11, FOUND YR 2048
        OMG 12, FOUND YR 4096
        OMG 13, FOUND YR 8192
        OMG 14, FOUND YR 16384
        OMG 15, FOUND YR 32768
        OMG 16, FOUND YR 65536
        OMG 17, FOUND YR 131072
        OMG 18, FOUND YR 262144
        OMG 19, FOUND YR 524288
        OMG 20, FOUND YR 1048576
        OMG 21, FOUND YR 2097152
        OMG 22, FOUND YR 4194304
        OMG 23, FOUND YR 8388608
        OMG 24, FOUND YR 16777216
        OMG 25, FOUND YR 33554432
        OMG 26, FOUND YR 67108864
        OMG 27, FOUND YR 134217728
        OMG 28, FOUND YR 268435456
        OMG 29, FOUND YR 536870912
        OMG 30, FOUND YR 1073741824
        OMG 31, FOUND YR 2147483648
    OIC
IF U SAY SO

O HAI IM SieveBase
    I HAS A size ITZ 0
    I HAS A max_index ITZ 0
    I HAS A word_count ITZ 0
    I HAS A data ITZ A BUKKIT

    HOW IZ I bit_index YR odd_value
        FOUND YR QUOSHUNT OF odd_value AN 2
    IF U SAY SO

    HOW IZ I init YR limit
        I HAS A words ITZ A BUKKIT
        I HAS A idx ITZ 0
        ME'Z size R limit
        ME'Z max_index R QUOSHUNT OF DIFF OF limit AN 1 AN 2
        ME'Z word_count R QUOSHUNT OF SUM OF ME'Z max_index AN 32 AN 32
        BTW Each pass gets a fresh SieveBase instance and a fresh backing BUKKIT.
        IM IN YR fill
            BOTH SAEM idx AN ME'Z word_count, O RLY?
                YA RLY
                    GTFO
            OIC
            words HAS A SRS idx ITZ 0
            idx R SUM OF idx AN 1
        IM OUTTA YR fill
        ME'Z data R words
    IF U SAY SO

    HOW IZ I run
        I HAS A data ITZ ME'Z data
        I HAS A factor_idx ITZ 1
        I HAS A factor ITZ 3
        I HAS A square_idx ITZ 4
        BTW Packed flags use inverted logic: 0 means still potentially prime, 1 means composite.
        IM IN YR sieve_loop
            DIFFRINT square_idx AN SMALLR OF square_idx AN ME'Z max_index, O RLY?
                YA RLY
                    GTFO
            OIC

            I HAS A multiple_idx ITZ square_idx
            IM IN YR mark_composite_loop
                DIFFRINT multiple_idx AN SMALLR OF multiple_idx AN ME'Z max_index, O RLY?
                    YA RLY
                        GTFO
                OIC
                I HAS A word_idx ITZ QUOSHUNT OF multiple_idx AN 32
                I HAS A bit_offset ITZ MOD OF multiple_idx AN 32
                I HAS A mask ITZ I IZ select_mask YR bit_offset MKAY
                I HAS A current_word ITZ data'Z SRS word_idx
                BOTH SAEM MOD OF QUOSHUNT OF current_word AN mask AN 2 AN 0, O RLY?
                    YA RLY
                        data'Z SRS word_idx R SUM OF current_word AN mask
                OIC
                multiple_idx R SUM OF multiple_idx AN factor
            IM OUTTA YR mark_composite_loop

            IM IN YR find_loop
                factor_idx R SUM OF factor_idx AN 1
                I HAS A word_idx ITZ QUOSHUNT OF factor_idx AN 32
                I HAS A bit_offset ITZ MOD OF factor_idx AN 32
                I HAS A mask ITZ I IZ select_mask YR bit_offset MKAY
                I HAS A current_word ITZ data'Z SRS word_idx
                BOTH SAEM MOD OF QUOSHUNT OF current_word AN mask AN 2 AN 0, O RLY?
                    YA RLY
                        factor R SUM OF PRODUKT OF factor_idx AN 2 AN 1
                        I HAS A factor_idx_plus ITZ SUM OF factor_idx AN 1
                        square_idx R PRODUKT OF PRODUKT OF factor_idx AN factor_idx_plus AN 2
                        GTFO
                OIC
            IM OUTTA YR find_loop
        IM OUTTA YR sieve_loop
    IF U SAY SO

    HOW IZ I count_primes
        I HAS A data ITZ ME'Z data
        I HAS A composite_count ITZ 0
        I HAS A word_idx ITZ 0
        IM IN YR count_loop
            BOTH SAEM word_idx AN ME'Z word_count, O RLY?
                YA RLY
                    GTFO
            OIC
            I HAS A current_word ITZ data'Z SRS word_idx
            composite_count R SUM OF composite_count AN POP8'Z SRS MOD OF current_word AN 256
            current_word R QUOSHUNT OF current_word AN 256
            composite_count R SUM OF composite_count AN POP8'Z SRS MOD OF current_word AN 256
            current_word R QUOSHUNT OF current_word AN 256
            composite_count R SUM OF composite_count AN POP8'Z SRS MOD OF current_word AN 256
            current_word R QUOSHUNT OF current_word AN 256
            composite_count R SUM OF composite_count AN POP8'Z SRS MOD OF current_word AN 256
            word_idx R SUM OF word_idx AN 1
        IM OUTTA YR count_loop
        FOUND YR SUM OF DIFF OF ME'Z max_index AN composite_count AN 1
    IF U SAY SO
KTHX

HOW IZ I pad_six YR value
    I HAS A out ITZ SMOOSH value MKAY
    IM IN YR pad_loop
        BOTH SAEM I IZ STRING'Z LEN YR out MKAY AN 6, O RLY?
            YA RLY
                GTFO
        OIC
        out R SMOOSH "0" AN out MKAY
    IM OUTTA YR pad_loop
    FOUND YR out
IF U SAY SO

HOW IZ I format_seconds YR elapsed_usec
    I HAS A whole ITZ QUOSHUNT OF elapsed_usec AN 1000000
    I HAS A frac ITZ MOD OF elapsed_usec AN 1000000
    FOUND YR SMOOSH whole AN "." AN I IZ pad_six YR frac MKAY MKAY
IF U SAY SO

I HAS A start_usec ITZ I IZ CLOCK'Z NAO YR 0 MKAY
I HAS A elapsed_usec ITZ 0
I HAS A passes ITZ 0
I HAS A prime_count ITZ 0
I HAS A valid ITZ WIN
I HAS A last_sieve

IM IN YR benchmark
    I HAS A sieve ITZ LIEK A SieveBase
    I IZ sieve'Z init YR SIEVE_SIZE MKAY
    I IZ sieve'Z run MKAY
    last_sieve R sieve
    passes R SUM OF passes AN 1
    elapsed_usec R DIFF OF I IZ CLOCK'Z NAO YR 0 MKAY AN start_usec
    BOTH SAEM BIGGR OF elapsed_usec AN TARGET_USEC AN elapsed_usec, O RLY?
        YA RLY
            GTFO
        NO WAI
    OIC
IM OUTTA YR benchmark

prime_count R I IZ last_sieve'Z count_primes MKAY
DIFFRINT prime_count AN EXPECTED_COUNT, O RLY?
    YA RLY
        valid R FAIL
    NO WAI
OIC

BOTH SAEM valid AN WIN, O RLY?
    YA RLY
        VISIBLE SMOOSH "cwager_lolcode;" AN passes AN ";" AN I IZ format_seconds YR elapsed_usec MKAY AN ";1;algorithm=base,faithful=yes,bits=1" MKAY
    NO WAI
OIC
KTHXBYE
