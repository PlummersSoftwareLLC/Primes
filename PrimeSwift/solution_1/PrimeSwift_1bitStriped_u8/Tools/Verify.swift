// Correctness checks for PrimeSieve against one independent Boolean sieve: complete
// prime lists at every limit from -2 through 30,000, at the alignment, group and
// tail boundaries of every dense and sparse handler, at 500 seeded random limits and
// every limit within 3 of a prime square up to 2,000,000, and at one million (78,498
// primes) and ten million (664,579), plus the storage layout cases. Run from the
// package directory, with AddressSanitizer and with the benchmark's flags:
//
//   swiftc -O -sanitize=address Sources/PrimeSieveSwift/PrimeSieve.swift Tools/Verify.swift -o .build/verify-asan && .build/verify-asan
//   swiftc -O -whole-module-optimization Sources/PrimeSieveSwift/PrimeSieve.swift Tools/Verify.swift -o .build/verify && .build/verify

struct SplitMix64 {
    var state: UInt64
    mutating func next() -> UInt64 {
        state &+= 0x9E37_79B9_7F4A_7C15
        var z = state
        z = (z ^ (z >> 30)) &* 0xBF58_476D_1CE4_E5B9
        z = (z ^ (z >> 27)) &* 0x94D0_49BB_1331_11EB
        return z ^ (z >> 31)
    }
}

@main
struct Verify {
    static let maxLimit = 10_000_000
    static let boundaryLimit = 2_000_000

    static func verifyStorageLayout() {
        // Explicit logical counts cover empty storage and both sides of byte
        // boundaries. The final case also checks the benchmark's observer range.
        let cases: [(limit: Int, odds: Int, bytes: Int)] = [
            (-2, 0, 0), (-1, 0, 0), (0, 0, 0), (1, 0, 0), (2, 0, 0),
            (3, 1, 1), (4, 1, 1), (5, 2, 1),
            (16, 7, 1), (17, 8, 1), (18, 8, 1), (19, 9, 2), (20, 9, 2),
            (31, 15, 2), (32, 15, 2), (33, 16, 2), (34, 16, 2),
            (35, 17, 3), (36, 17, 3),
            (255, 127, 16), (256, 127, 16), (257, 128, 16), (258, 128, 16),
            (259, 129, 17), (1_000_000, 499_999, 62_500),
        ]
        for test in cases {
            let layout = PrimeSieve.storageLayout(for: test.limit)
            precondition(layout.oddCount == test.odds && layout.byteCount == test.bytes,
                         "Incorrect storage layout at \(test.limit)")
            let sieve = PrimeSieve(limit: test.limit)
            sieve.withStorage { pointer in
                precondition((pointer == nil) == (test.bytes == 0))
                for byte in 0..<test.bytes {
                    precondition(pointer!.load(fromByteOffset: byte, as: UInt8.self) == 0)
                }
            }
        }
        // Extreme limits exercise arithmetic only, never enormous allocations.
        let empty = PrimeSieve.storageLayout(for: Int.min)
        precondition(empty.oddCount == 0 && empty.byteCount == 0)
        let largest = PrimeSieve.storageLayout(for: Int.max)
        precondition(largest.oddCount == Int.max / 2 && largest.byteCount == Int.max / 16 + 1)
        print("Passed: 25 storage layout/zeroing cases and Int.min/Int.max layout arithmetic.")
    }

    static func main() {
        verifyStorageLayout()

        // One reference sieve serves every check below.
        var composite = [Bool](repeating: false, count: maxLimit + 1)
        var p = 2
        while p * p <= maxLimit {
            if !composite[p] {
                for m in stride(from: p * p, through: maxLimit, by: p) { composite[m] = true }
            }
            p += 1
        }
        let allPrimes = (2...maxLimit).filter { !composite[$0] }
        func expected(_ limit: Int) -> ArraySlice<Int> {
            var lo = 0, hi = allPrimes.count
            while lo < hi {
                let mid = (lo + hi) / 2
                if allPrimes[mid] <= limit { lo = mid + 1 } else { hi = mid }
            }
            return allPrimes[..<lo]
        }
        var checks = 0
        func check(_ limit: Int, repeating: Bool = false) {
            let sieve = PrimeSieve(limit: limit)
            sieve.runSieve()
            let actual = sieve.primes()
            precondition(actual.elementsEqual(expected(limit)), "Incorrect primes at \(limit)")
            if repeating {
                // Marking again must preserve the same result.
                sieve.runSieve()
                precondition(sieve.primes() == actual, "Repeated marking changed the result at \(limit)")
            }
            checks += 1
        }

        // Every small limit, including empty and one-byte sieves, then every limit
        // where dense word groups first appear, with every tail length.
        for limit in -2...30_000 { check(limit, repeating: limit <= 2_048) }

        // The 128-bit handlers can begin full groups beyond 30,000. Include
        // alignment and first/second group edges for every odd handled value.
        // A group's byte-rounded storage exists at limit 2*endBit-13, before
        // every flag in its last byte is valid; also test 2*endBit+1 explicitly.
        var vectorLimits = Set<Int>()
        for factor in stride(from: 65, through: 127, by: 2) {
            var aligned = (factor * factor - 3) / 2
            while aligned & 127 != 0 { aligned += factor }
            let groupEnd = aligned + 128 * factor
            for boundary in [factor * factor, 2 * aligned + 3,
                             2 * groupEnd - 13, 2 * groupEnd + 1,
                             2 * (groupEnd + 128 * factor) - 13,
                             2 * (groupEnd + 128 * factor) + 1] {
                for offset in -2...2 { vectorLimits.insert(boundary + offset) }
            }
            // Exercise every possible next tail-mark position, including the
            // transition from 127 to 128 marks, on both sides of its number.
            for mark in 0..<128 {
                let nextNumber = 2 * (groupEnd + mark * factor) + 3
                for offset in -1...1 { vectorLimits.insert(nextNumber + offset) }
            }
        }
        for limit in vectorLimits.sorted() { check(limit) }

        // Entry and cleanup around the first four eight-mark groups for every
        // active sparse factor, at both the last mark itself and the earlier
        // limit where its final byte first exists, since padding is markable.
        var groupLimits = Set<Int>()
        for q in allPrimes where q > 63 && q * q <= boundaryLimit {
            let start = (q * q - 3) / 2
            for marks in [8, 16, 24, 32] {
                let lastBit = start + (marks - 1) * q
                for boundary in [2 * lastBit + 3, 16 * (lastBit >> 3) + 3] {
                    for delta in -1...1 where boundary + delta <= boundaryLimit {
                        groupLimits.insert(boundary + delta)
                    }
                }
            }
        }
        for limit in groupLimits.sorted() { check(limit) }

        // Both sides of every prime square, where a new factor starts marking.
        var squareLimits = 0
        for q in allPrimes where q * q <= boundaryLimit {
            for limit in (q * q - 3)...(q * q + 3) {
                check(limit)
                squareLimits += 1
            }
        }

        // Seeded random limits, and the large limits with their known counts.
        var rng = SplitMix64(state: 0x5EED)
        for _ in 0..<500 { check(2_049 + Int(rng.next() % UInt64(boundaryLimit - 2_049))) }
        for limit in [994_008, 994_009, 994_010, 1_000_000, 10_000_000] { check(limit, repeating: true) }
        precondition(expected(1_000_000).count == 78_498 && expected(10_000_000).count == 664_579)

        print("Passed: \(checks) complete prime lists: every limit -2...30,000, \(vectorLimits.count) 128-bit alignment/group/tail limits, \(groupLimits.count) sparse group and cleanup limits, \(squareLimits) limits within 3 of every prime square up to 2,000,000, 500 random limits up to 2,000,000, and the large limits including 1M (78,498 primes) and 10M (664,579).")
    }
}
