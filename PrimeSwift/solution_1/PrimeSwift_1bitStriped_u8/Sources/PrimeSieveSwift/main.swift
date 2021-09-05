import Foundation
import ArgumentParser

typealias Word = UInt8
let wordSize: Int = Word.bitWidth
let log2_wordSize = wordSize &- 1 &- Word(wordSize).leadingZeroBitCount
let modulus_wordSize = wordSize &- 1

// This should establish an array of masks for accessing individual bits from a Word that
// is built during compile.  Accessing this array is faster than computing the bit shift
// mask during each bit twiddling operation.
let bitMasks = UnsafeMutableBufferPointer<Word>.allocate(capacity: wordSize)
_ = bitMasks.initialize(from: ContiguousArray((0..<wordSize).lazy.map{ Word(1) &<< $0 }))
defer {
    bitMasks.deallocate()
}

// This is a very limited single bit Boolean Array
struct BooleanBitArray {
    private let wordArraySize: Int
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
    @inline(__always) internal func maskIndex(at index: Int) -> (Int, Int) {
        let wordIndex = index &>> log2_wordSize // faster than index / wordSize
        let subIndex = index & modulus_wordSize // faster than index % wordSize
        return (wordIndex, subIndex)
    }

    @inline(__always) func getBit(at index: Int) -> Bool {
        let (i, m) = maskIndex(at: index)
        return ( words[i] & bitMasks[m] ) != 0
    }

    @inline(__always) func setBit(at index: Int) {
        let (i, m) = maskIndex(at: index)
        words[i] |= bitMasks[m]
    }

    @inline(__always) func clearBit(at index: Int) {
        let (i, m) = maskIndex(at: index)
        words[i] &= ~bitMasks[m]
    }
    
    @inline(__always) func clearBitStriped(from start: Int, by skip: Int) {
        var stripeStartingIndex = start
        
        for _ in (0..<wordSize) {
            var (i, m) = maskIndex(at: stripeStartingIndex)
            
            let clearMask = ~bitMasks[m]
            while i < wordArraySize {
                words[i] &= clearMask
                i &+= skip
            }
            
            stripeStartingIndex &+= skip
        }
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
    private let sieveLimit: Int
    private let factorLimit: Int
    private let primeArray: BooleanBitArray

    init(limit: Int) {
        sieveLimit = limit
        factorLimit = iSqrt(limit)
        let arraySize = (limit &+ 1) &>> 1
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
        
        var factorIndex = -1
        repeat {
            factorIndex &+= 1
            if !primeArray.getBit(at: factorIndex) { continue }
            
            let factor = number(at: factorIndex)
            let nonPrimeIndex = index(for: factor &* factor)
            
            primeArray.clearBitStriped(from: nonPrimeIndex, by: factor)
            
        } while factorIndex <= factorIndexLimit
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
        print("yellowcub_striped_UInt8;\(passes);\(duration);1;algorithm=base,faithful=yes,bits=1\n")
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
