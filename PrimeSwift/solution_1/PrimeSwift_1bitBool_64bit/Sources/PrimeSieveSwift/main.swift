import Foundation
import ArgumentParser

// This is a very limited single bit Boolean Array
struct BooleanBitArray {
    typealias Word = UInt
    let wordArraySize: Int
    private let wordSize: Int = 8*MemoryLayout<Word>.size
    private let log2_wordSize = 8*MemoryLayout<Word>.size &- 1 &- Word(8*MemoryLayout<Word>.size).leadingZeroBitCount
    
    private let words: UnsafeMutableBufferPointer<Word>
    
    init(repeating value: Bool, count arraySize: Int){
        wordArraySize = (arraySize &+ (wordSize &- 1)) &>> log2_wordSize
        words = UnsafeMutableBufferPointer<Word>.allocate(capacity: wordArraySize)
        words.initialize(repeating: value ? Word.max : Word(0))
    }

    func deallocate(){
        words.deallocate()
    }

    // Returns the array word index and sub index for an input bit index
    //   - This takes advantage of wordSize being a power of 2 to compute
    //     the modulus (%) faster by masking wordSize -1
    @inline(__always) internal func maskIndex(at index: Int) -> (Int, Int) {
        // a faster implementation of index / wordSize by shifting by log2(wordSize)
        let wordIndex = index &>> log2_wordSize
        let subIndex = index & (wordSize &- 1)
        return (wordIndex, subIndex)
    }

    @inline(__always) func getBit(at index: Int) -> Bool {
        let (i, m) = maskIndex(at: index)
        return (words[i] & (Word(1) &<< m) ) != 0
    }

    @inline(__always) func setBit(at index: Int) {
        let (i, m) = maskIndex(at: index)
        words[i] |= (Word(1) &<< m)
    }

    @inline(__always) func clearBit(at index: Int) {
        let (i, m) = maskIndex(at: index)
        words[i] &= ~(Word(1) &<< m)
    }

}

func iSqrt(_ x: Int) -> Int {
    if x == 0 { return 0 }
    let z = x &>> 2
    let r2 = iSqrt(z) &<< 1
    let r3 = r2 &+ 1
    if x < (r3 &* r3) { return r2 }
    return r3
}

class Sieve {
    let sieveLimit: Int
    private let factorLimit: Int
    let arraySize: Int
    
    var primeArray: BooleanBitArray

    init(limit: Int) {
        sieveLimit = limit
        factorLimit = iSqrt(limit)
        arraySize = (limit &+ 1) &>> 1
        primeArray = BooleanBitArray(repeating: true, count: arraySize)
    }

    deinit {
        primeArray.deallocate()
    }

    @inline(__always) internal func index(for num: Int) -> Int {
        return num &>> 1 &- 1 // {3, 5, 7, ...} -> {0, 1, 2, ...}
    }
    
    @inline(__always) internal func number(at index: Int) -> Int {
        return index &<< 1 &+ 3 // {0, 1, 2, ...} -> {3, 5, 7, ...}
    }

    func runSieve() {
        let factorIndexLimit = index(for: factorLimit)
        let nonPrimeIndexLimit = index(for: sieveLimit)
        
        var factorIndex = -1
        repeat {
            factorIndex &+= 1
            if !primeArray.getBit(at: factorIndex) { continue }
            
            let factor = number(at: factorIndex)
            var nonPrimeIndex = index(for: factor &* factor)
            
            repeat {
                primeArray.clearBit(at: nonPrimeIndex)
                nonPrimeIndex &+= factor
            } while ( nonPrimeIndex <= nonPrimeIndexLimit )
        } while factorIndex < factorIndexLimit
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
            if primeArray.getBit(at: index(for: num)) {
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
                if primeArray.getBit(at: index(for: num)) {
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
