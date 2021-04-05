import Foundation

protocol BitArray {
    init(size: Int)
    func getBit(_ idx: Int) -> Bool
    mutating func clearBit(_ idx: Int)
}

struct BoolBitArray: BitArray {
    var rawBits: [Bool]
    init(size: Int) {
        rawBits = Array(repeating: true, count: (size + 1) / 2)
    }

    /// Gets a bit from the array of bits, but automatically just filters out even numbers as false,
    /// and then only uses half as many bits for actual storage
    func getBit(_ idx: Int) -> Bool {
        assert(idx % 2 != 0)
        return rawBits[idx / 2]
    }

    /// Reciprocal of GetBit, ignores even numbers and just stores the odds.
    /// Since the prime sieve work should never waste time clearing even numbers,
    /// this code will assert if you try to
    mutating func clearBit(_ idx: Int) {
        assert(idx % 2 != 0, "If you're setting even bits, you're sub-optimal for some reason!")
        rawBits[idx / 2] = false
    }
}

class PrimeSieveSwift {
    let sieveSize: Int
    var bits: BitArray

    init(limit: Int) {
        sieveSize = limit
        bits = BoolBitArray(size: limit)
    }

    /// Return the count of bits that are still set in the sieve.
    /// Assumes you've already called `runSieve`, of course!
    func countPrimes() -> Int {
        var count = 1
        for num in stride(from: 3, to: sieveSize, by: 2) {
            if bits.getBit(num) {
                count += 1
            }
        }
        return count
    }

    func runSieve() {
        var factor = 3
        let q = Int(sqrt(Double(sieveSize)))

        while factor <= q {
            for num in stride(from: factor, to: sieveSize, by: 2) {
                if bits.getBit(num) {
                    factor = num
                    break
                }
            }
            for num in stride(from: factor * factor, to: sieveSize, by: factor * 2) {
                bits.clearBit(num)
            }
            factor += 2
        }
    }
}


extension PrimeSieveSwift {
    /// Historical data for validating our results - the number of primes to be found
    /// under some limit, such as 168 primes under 1000
    static let primeCounts = [
        10: 4,
        100: 25,
        1000: 168,
        10000: 1229,
        100000: 9592,
        1000000: 78498,
        10000000: 664579,
        100000000: 5761455,
    ]

    /// Look up our count of primes in the historical data (if we have it) to see if it matches
    func validateResults() -> Bool {
        guard let correctCount = Self.primeCounts[sieveSize] else {
            return false
        }
        return correctCount == countPrimes()
    }
}

extension PrimeSieveSwift {
    func printResults(showingNumbers: Bool, duration: TimeInterval, passes: Int) {
        if showingNumbers {
            print("2, ", terminator: "")
        }

        var count = 1
        for num in stride(from: 3, to: sieveSize, by: 2) {
            if bits.getBit(num) {
                if showingNumbers {
                    print(num, terminator: ", ")
                }
                count += 1
            }
        }

        assert(count == countPrimes())

        print("\nPasses: \(passes), Time: \(duration), Avg: \(duration/Double(passes)), Limit: \(sieveSize), Count: \(count), Valid: \(validateResults())")
    }
}

let start = Date()
var passes = 0
var sieve: PrimeSieveSwift!

while start.distance(to: Date()) < 10 {
    sieve = PrimeSieveSwift(limit: 1_000_000)
    sieve.runSieve()
    passes += 1
}

sieve.printResults(showingNumbers: false, duration: start.distance(to: Date()), passes: passes)
