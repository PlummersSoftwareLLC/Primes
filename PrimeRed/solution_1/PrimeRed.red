Red [
    Author: "mmcdon20"
    Purpose: "A Red implementation of David Plummer's PrimeSieve application."
]

PrimeSieve: func [size [integer!] return: [object!]] [
    make object! [
        sieveSize: size
        bits: complement make bitset! ((size + 1) >> 1)

        resultsDictionary: #(
            10 4
            100 25
            1000 168
            10000 1229
            100000 9592
            1000000 78498
            10000000 664579
            100000000 5761455
            1000000000 50847534
            10000000000 455052511
        )

        validateResults: func [return: [logic!]] [
            resultsDictionary/:sieveSize = countPrimes
        ]

        getBit: func [index [integer!] return: [logic!]] [
            bits/(:index >> 1)
        ]

        clearBit: func [index [integer!] return: [logic!]] [
            bits/(:index >> 1): false
        ]

        runSieve: func [return: [none!]] [
            factor: 3
            q: round/down (sqrt sieveSize)

            while [factor <= q] [
                num: factor
                while [num < sieveSize] [
                    if getBit num [
                        factor: num
                        break
                    ]
                    num: num + 2
                ]

                num: factor * factor
                while [num < sieveSize] [
                    clearBit num
                    num: num + (factor * 2)
                ]

                factor: factor + 2
            ]
        ]

        printResults: func [
            showResults [logic!]
            duration [float!] 
            passes [integer!] 
            return: [none!]
        ] [
            if showResults [
                write-stdout "2, "
            ]

            count: if sieveSize >= 2 [ 1 ][ 0 ]

            num: 3
            while [num < sieveSize] [
                if getBit num [
                    if showResults [
                        write-stdout rejoin [num ", "]
                    ]
                    count: count + 1
                ]
                num: num + 2
            ]

            if showResults [
                write-stdout "^/"
                write-stdout rejoin ["Passes: " passes ", Time: " duration ", "]
                write-stdout rejoin ["Avg: " (duration / passes) ", Limit: " sieveSize ", "]
                write-stdout rejoin ["Count1: " count ", Count2: " countPrimes ", "]
                write-stdout rejoin ["Valid: " validateResults "^/"]
            ]

            write-stdout rejoin ["mmcdon20_red;" passes ";" duration ";1;algorithm=base,faithful=yes,bits=1^/"]
        ]

        countPrimes: func [return: [integer!]] [
            count: if sieveSize >= 2 [ 1 ][ 0 ]
            i: 3
            while [i < sieveSize] [
                if getBit i [
                    count: count + 1
                ]
                i: i + 2
            ]
            count
        ]
    ]
]

passes: 0
start: to float! now/precise/time

forever [
    sieve: PrimeSieve 1000000
    do [sieve/runSieve]
    passes: passes + 1
    stop: to float! now/precise/time

    if (stop - start) >= 5.0 [
        do [sieve/printResults false (stop - start) passes]
        break
    ]
]
