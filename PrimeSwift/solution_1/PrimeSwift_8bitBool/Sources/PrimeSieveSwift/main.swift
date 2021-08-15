import Foundation
import ArgumentParser

class Sieve {
    let sieveSize: Int
    let factorLimit: Int
    var bits: UnsafeMutableBufferPointer<Bool>

    init(limit: Int) {
        sieveSize = limit
        factorLimit = Int(sqrt(Double(sieveSize)))
        bits = UnsafeMutableBufferPointer<Bool>.allocate(capacity: (limit + 1) / 2)
        bits.initialize(repeating: true)
    }

    deinit { bits.deallocate() }

    @inline(__always) internal func index(for num: Int) -> Int {
        return num / 2 &- 1 // {3, 5, 7, ...} -> {0, 1, 2, ...}
    }
    
    @inline(__always) internal func number(at index: Int) -> Int {
        return index &* 2 &+ 3 // {0, 1, 2, ...} -> {3, 5, 7, ...}
    }

    func runSieve() {
        let factorIndexLimit = index(for: factorLimit)
        let nonPrimeIndexLimit = index(for: sieveSize)
        
        var factorIndex = -1
        repeat {
            factorIndex &+= 1
            if !bits[factorIndex] { continue }
            
            let factor = number(at: factorIndex)
            var nonPrimeIndex = index(for: factor &* factor)
            
            repeat {
                bits[nonPrimeIndex] = false
                nonPrimeIndex &+= factor
            } while ( nonPrimeIndex <= nonPrimeIndexLimit )
        } while factorIndex < factorIndexLimit
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
            if bits[index(for: num)] {
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
                if bits[index(for: num)] {
                    print(num, terminator: ", ")
                }
            }
        }

        print("\nPasses: \(passes), Time: \(duration), Avg: \(duration/Double(passes)), Limit: \(sieveSize), Count: \(countPrimes()), Valid: \(validateResults())")
        
        /// Following 2 lines added by rbergen to conform to drag race output format
        print()
        print("j-f1_yellowcub_bool;\(passes);\(duration);1;algorithm=base,faithful=yes,bits=\(8*MemoryLayout<Bool>.size)\n")
    }
}

struct StopWatch {
    private let start =  Date()
    public var stop = 0.0

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
    private var maxTime = 5.0

    @Option(name: [.customLong("list-results"), .customShort("l")], help: "List all computed primes.")
    private var listResults = false

    func run() throws {
        var passes = 0
        
        var stopWatch = StopWatch()
        repeat {
            let sieve = Sieve(limit: upperLimit)
            sieve.runSieve()
            passes &+= 1
            
            if stopWatch.lap() < maxTime { continue }
            
            sieve.printResults(showingNumbers: listResults, duration: stopWatch.stop, passes: passes)
            break
        } while true
        
    }

}
PrimeSieveSwift.main()
