time_limit := 5.
limit := 1000000.
"0=off, 1=on"
show_results := 0.

Object subclass: BitArray [
    | byte_array byte_size bit_size |

    init: bitCount initval: initValue [
        byte_size := bitCount + 7 // 8.
        bit_size := bitCount.
        byte_array := ByteArray new: byte_size.

        (initValue == 1 )
            ifTrue: [
                1 to: byte_size do: [:i |
                    byte_array at: i put: 16rFF.
                ]
            ]
    ]  

    bitAt: bitIndex [
        | bitPosition index |

        index := bitIndex - 1 // 8 + 1.
        bitPosition := bitIndex - 1 \\ 8 + 1.
        
        ^ (byte_array at: index) bitAt: bitPosition
    ]

    setBit: bitIndex [
        | bitPosition index |

        index := bitIndex - 1 // 8 + 1.
        bitPosition := bitIndex - 1 \\ 8 + 1.
        byte_array at: index put: ((byte_array at: index) setBit: bitPosition)
    ]

    clearBit: bitIndex [
        | bitPosition index |

        index := bitIndex - 1 // 8 + 1.
        bitPosition := bitIndex - 1 \\ 8 + 1.
        byte_array at: index put: ((byte_array at: index) clearBit: bitPosition)
    ]

    searchNextBit: bitIndex [
        | bitPosition index |

        (bitIndex + 1) to: bit_size do: [ :cur_index |
            ( (self bitAt: cur_index) == 1)
                ifTrue: [
                    ^cur_index.
                ].
        ].
        "not found / last reached return -1"
        ^-1.
    ]

    countBits [
        | count |

        count := 0.
        1 to: bit_size do: [ :cur_index |
            ( (self bitAt: cur_index) == 1)
                ifTrue: [
                    count := count + 1.
                ].
        ].
        ^count.
    ]
]

BitArray class extend [ 
    new: bitCount initval: initValue [ 
        | r |

        r := (super new).
        r init: bitCount initval: initValue.  
        ^r
    ]
]

Object subclass: PrimeSieve [
    | bit_array limit bit_size|

    init: max_limit [
        limit := max_limit.
        
        bit_size := limit // 2.
        ( (limit \\ 2) == 0)
            ifTrue: [
                bit_size := bit_size - 1.
            ].
        bit_array := BitArray new: bit_size initval: 1.
    ]

    run_sieve [
        | maxroot maxroot_index factor  |

        maxroot := (limit sqrt) floor.
        maxroot_index := maxroot // 2.
        ((maxroot \\ 2) == 0)
            ifTrue: [
                maxroot_index := maxroot_index - 1.   
            ].
        factor := 1.

        [factor <= maxroot_index] whileTrue: [
            |prime start step|

            prime := factor * 2 + 1.
            start := ( ((prime * prime) - 1) // 2 ).
            step  := factor * 2 + 1.
            "Crossout"
            start to: bit_size by: step do: [:index|
                bit_array clearBit: index.
            ].
            "Find next"
            factor := bit_array searchNextBit: factor.
        ].
    ]

    count_primes [
        ^((bit_array countBits) + 1).
    ]

    bit_array_to_primes [
        | primes count i|

        count := self count_primes.
        primes := Array new: count.
        primes at: 1 put: 2.

        i:=2.
        1 to: bit_size do: [ :cur_index |
            ( (bit_array bitAt: cur_index) == 1)
                ifTrue: [
                    primes at: i put: (cur_index * 2 + 1).
                    i := i + 1.
                ].
        ].
        ^primes.
    ]

    validate_results: count [
        |knownPrimeCounts|

        knownPrimeCounts := Dictionary new.
        knownPrimeCounts at: 10 put: 4.
        knownPrimeCounts at: 100 put: 25.
        knownPrimeCounts at: 1000 put: 168.
        knownPrimeCounts at: 10000 put: 1229.
        knownPrimeCounts at: 100000 put: 9592.
        knownPrimeCounts at: 1000000 put: 78498.
        knownPrimeCounts at: 10000000 put: 664579.
        knownPrimeCounts at: 100000000 put: 5761455.

        ^((knownPrimeCounts at: limit ifAbsent: [0]) == count).
    ]

    print_results: show_results time: duration nr: passes [
        |avg count valid|

        avg := duration / passes.
        count := self count_primes.
        valid := self validate_results: count.

        (show_results == 1)
            ifTrue: [
                (self bit_array_to_primes) displayNl.
            ].
        
        (   'Passes: ', passes displayString, 
          ', Time: ',(duration asFloatE) displayString,
          ', Avg: ', (avg asFloatE) displayString,' (sec/pass)',
          ', Limit: ', limit displayString,
          ', Count: ', count displayString, 
          ', Valid: ', valid displayString
        ) displayNl.
        
        '' displayNl.

        (   'fvbakel_smalltalk;',passes displayString,
            ';',(duration asFloatE) displayString,
            ';1;algorithm=base,faithful=yes,bits=1'
        ) displayNl.
    ]
]

PrimeSieve class extend [ 
    new: max_limit [ 
        | r |

        r := (super new).
        r init: max_limit.  
        ^r
    ]
]

"Main function"

max_limit_ms := (time_limit * 1000).
start := Time millisecondClockValue.

duration_ms := 0.
passes := 0.
[ duration_ms < max_limit_ms ] whileTrue: [
    passes := passes + 1.
    sieve := PrimeSieve new: limit.
    sieve run_sieve.

    duration_ms := (Time millisecondClockValue) - start.

    (duration_ms >= max_limit_ms)
        ifTrue: [
            duration := duration_ms / 1000.
            sieve print_results: show_results time: duration nr: passes.
        ].
].
