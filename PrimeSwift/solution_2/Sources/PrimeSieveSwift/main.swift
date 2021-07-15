import Foundation
import ArgumentParser

class Sieve {
    let sieveSize: Int
    let q: Int
    var bits: UnsafeMutableBufferPointer<Bool>

    init(limit: Int) {
        sieveSize = limit
        q = Int(sqrt(Double(sieveSize)))
        bits = UnsafeMutableBufferPointer<Bool>.allocate(capacity: (limit + 1) / 2)
        bits.initialize(repeating: true)
    }

    deinit { bits.deallocate() }

    @inlinable func index(_ num: Int) -> Int { return num >> 1 - 1}

    /// Does @inlinable help here?
    @inlinable func runSieve() {
        var factor = 3
        while factor <= q {
            while !bits[index(factor)] {
                factor += 2
            }
            var nFactor = factor*factor
            while nFactor <= sieveSize {
                bits[index(nFactor)] = false
                nFactor += 2*factor
            }
            factor += 2
        }
    }
}


extension Sieve {
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

    /// Return the count of bits that are still set in the sieve.
    /// Assumes you've already called `runSieve`, of course!
    func countPrimes() -> Int {
        var count = 1
        for num in stride(from: 3, to: sieveSize, by: 2) {
            if bits[index(num)] {
                count += 1
            }
        }
        return count
    }

    /// Look up our count of primes in the historical data (if we have it) to see if it matches
    func validateResults() -> Bool {
        guard let correctCount = Self.primeCounts[sieveSize] else {
            return false
        }
        return correctCount == countPrimes()
    }
}

extension Sieve {
    func printResults(showingNumbers: Bool, duration: TimeInterval, passes: Int) {
        if showingNumbers {
            print("2, ", terminator: "")
            for num in stride(from: 3, to: sieveSize, by: 2) {
                if bits[index(num)] {
                    print(num, terminator: ", ")
                }
            }
        }

        print("\nPasses: \(passes), Time: \(duration), Avg: \(duration/Double(passes)), Limit: \(sieveSize), Count: \(countPrimes()), Valid: \(validateResults())")
        
        /// Following 2 lines added by rbergen to conform to drag race output format
        print()
        print("yellowcub;\(passes);\(duration);1;algorithm=base,faithful=yes,bits=\(MemoryLayout<Bool>.size)\n")
    }
}

struct StopWatch {
    private let start: Date
    public var stop = 0.0

    init(){ start = Date() }

    mutating func lap() -> Double {
        stop = start.distance(to: Date())
        return stop
    }
}

struct PrimeSieveSwift: ParsableCommand {
    public static let configuration = CommandConfiguration(abstract: "Generate Primes")

    @Option(name: [.customLong("upper-limit"), .customShort("n")], help: "Compute all primes below this limit.")
    private var upperLimit = 1_000_000

    @Option(name: [.customLong("time"), .customShort("t")], help: "Max time for number of passes.")
    private var maxTime = 10.0

    @Option(name: [.customLong("list-results"), .customShort("l")], help: "List all computed primes.")
    private var listResults = false

    func run() throws {
        var passes = 0
        let sieve = Sieve(limit: upperLimit)

        var stopWatch = StopWatch()
        while stopWatch.lap() < maxTime {
            sieve.runSieve()
            passes += 1
        }

        sieve.printResults(showingNumbers: listResults, duration: stopWatch.stop, passes: passes)

    }
}
PrimeSieveSwift.main()
