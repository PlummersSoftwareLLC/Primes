import Foundation
import ArgumentParser

// This is a very limited single bit Boolean Array
struct BooleanBitArray<Word> where Word: FixedWidthInteger {
    private let words: UnsafeMutableBufferPointer<Word>
    private let wordSize: Int = 8*MemoryLayout<Word>.size
    
    init(repeating value: Bool, count arraySize: Int){
        let wordArraySize = (arraySize + (wordSize - 1)) / wordSize
        words = UnsafeMutableBufferPointer<Word>.allocate(capacity: wordArraySize)
        words.initialize(repeating: value ? Word.max : 0)
    }

    func deallocate(){
        words.deallocate()
    }

    @usableFromInline internal func maskIndex(_ num: Int) -> (Int, Word) {
        let wordIndex = num / wordSize
        let mask = Word(1) << (num % wordSize)
        return (wordIndex, mask)
    }

    @inlinable func getBit(atIndex num: Int) -> Bool {
        let (i, m) = maskIndex(num)
        return (words[i] & m) != 0
    }

    @inlinable func setBit(atIndex num: Int, to value: Bool) {
        let (i, m) = maskIndex(num)
        if value { words[i] |= m } else { words[i] &= ~m }
    }

}

class Sieve {
    let sieveSize: Int
    let factorLimit: Int
    var primeArray: BooleanBitArray<UInt>

    init(limit: Int) {
        sieveSize = limit
        factorLimit = Int(sqrt(Double(sieveSize)))
        primeArray = BooleanBitArray(repeating: true, count: (limit + 1) / 2)
    }

    deinit {
        primeArray.deallocate()
    }

    func index(for num: Int) -> Int {
        assert(num % 2 == 1, "Error: BitArray:get property should not have accessed an even number.")
        return num >> 1 - 1 // 3 -> 0, 5 -> 1, 7 -> 2, etc.
    }

    func runSieve() {
        var factor = 3
        while factor <= factorLimit {
            while !primeArray.getBit(atIndex: index(for: factor)) {
                factor += 2
            }
            var nFactor = factor*factor
            while nFactor <= sieveSize {
                primeArray.setBit(atIndex: index(for: nFactor), to: false)
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
                 10:         4,
                100:        25,
              1_000:       168,
             10_000:     1_229,
            100_000:     9_592,
          1_000_000:    78_498,
         10_000_000:   664_579,
        100_000_000: 5_761_455,
    ]

    /// Return the count of bits that are still set in the sieve.
    /// Assumes you've already called `runSieve`, of course!
    func countPrimes() -> Int {
        var count = 1
        for num in stride(from: 3, to: sieveSize, by: 2) {
            if primeArray.getBit(atIndex: index(for: num)) {
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
            print(2, terminator: ", ")
            for num in stride(from: 3, to: sieveSize, by: 2) {
                if primeArray.getBit(atIndex: index(for: num)) {
                    print(num, terminator: ", ")
                }
            }
        }

        print("\nPasses: \(passes), Time: \(duration), Avg: \(duration/Double(passes)), Limit: \(sieveSize), Count: \(countPrimes()), Valid: \(validateResults())")
        
        /// Following 2 lines added by rbergen to conform to drag race output format
        print()
        print("yellowcub_bit64;\(passes);\(duration);1;algorithm=base,faithful=yes,bits=1\n")
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
        while true {
            let sieve = Sieve(limit: upperLimit)
            sieve.runSieve()
            passes += 1

            if stopWatch.lap() >= maxTime {
                sieve.printResults(showingNumbers: listResults, duration: stopWatch.stop, passes: passes)
                break
            }
        }

    }

}
PrimeSieveSwift.main()
