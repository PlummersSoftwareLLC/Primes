import Foundation
import ArgumentParser

// This is a very limited single bit Boolean Array
struct BooleanBitArray {
    typealias Word = UInt
    let wordArraySize: Int
    private let wordSize: Int = 8*MemoryLayout<Word>.size
    
    private let words: UnsafeMutableBufferPointer<Word>
    
    init(repeating value: Bool, count arraySize: Int){
        wordArraySize = (arraySize + (wordSize - 1)) / wordSize
        words = UnsafeMutableBufferPointer<Word>.allocate(capacity: wordArraySize)
        words.initialize(repeating: value ? Word.max : Word(0))
    }

    func deallocate(){
        words.deallocate()
    }

    @inline(__always) internal func maskIndex(_ num: Int) -> (Int, Word) {
        let wordIndex = num / wordSize
        let mask = Word(1) << (num % wordSize)
        return (wordIndex, mask)
    }

    @inline(__always) func getBit(atIndex num: Int) -> Bool {
        let (i, m) = maskIndex(num)
        return (words[i] & m) != 0
    }

    @inline(__always) func setBit(atIndex num: Int) {
        let (i, m) = maskIndex(num)
        words[i] |= m
    }

    @inline(__always) func clearBit(atIndex num: Int) {
        let (i, m) = maskIndex(num)
        words[i] &= ~m
    }

}

class Sieve {
    let sieveLimit: Int
    private let factorLimit: Int
    let arraySize: Int
    
    var primeArray: BooleanBitArray

    init(limit: Int) {
        sieveLimit = limit
        factorLimit = Int(sqrt(Double(limit)))
        arraySize = (limit + 1) / 2
        primeArray = BooleanBitArray(repeating: true, count: arraySize)
    }

    deinit {
        primeArray.deallocate()
    }

    @inline(__always) internal func index(for num: Int) -> Int {
        return num / 2 - 1 // 3 -> 0, 5 -> 1, 7 -> 2, etc.
    }
    
    @inline(__always) internal func number(at index: Int) -> Int {
        return index * 2 + 3 // 0 -> 3, 1 -> 5, 2 -> 7, etc.
    }

    func runSieve() {
        var factorIndex = 0
        let factorIndexLimit = factorLimit / 2 - 1
        let nonPrimeIndexLimit = index(for: sieveLimit)
        
        while factorIndex <= factorIndexLimit {
            let factor = number(at: factorIndex)
            let isPrime = primeArray.getBit(atIndex: factorIndex)
            var nonPrimeIndex = index(for: factor*factor)
            
            while isPrime && ( nonPrimeIndex <= nonPrimeIndexLimit ) {
                primeArray.clearBit(atIndex: nonPrimeIndex)
                nonPrimeIndex += factor
            }
            factorIndex += 1
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
        for num in stride(from: 3, to: sieveLimit, by: 2) {
            if primeArray.getBit(atIndex: index(for: num)) {
                count += 1
            }
        }
        return count
    }

    /// Look up our count of primes in the historical data (if we have it) to see if it matches
    func validateResults() -> Bool {
        guard let correctCount = Self.primeCounts[sieveLimit] else {
            return false
        }
        return correctCount == countPrimes()
    }
}

extension Sieve {
    func printResults(showingNumbers: Bool, duration: TimeInterval, passes: Int) {
        if showingNumbers {
            print(2, terminator: ", ")
            for num in stride(from: 3, to: sieveLimit, by: 2) {
                if primeArray.getBit(atIndex: index(for: num)) {
                    print(num, terminator: ", ")
                }
            }
        }

        print("\nPasses: \(passes), Time: \(duration), Avg: \(duration/Double(passes)), Limit: \(sieveLimit), Count: \(countPrimes()), Valid: \(validateResults())")
        
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
