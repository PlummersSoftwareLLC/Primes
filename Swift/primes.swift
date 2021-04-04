import Foundation

class PrimeSieve {
    
    var sieveSize = 0
    var rawBits: UnsafeMutableRawBufferPointer
    
    let myDict = [
        10 : 4,
        100 : 25,
        1000 : 168,
        10000 : 1229,
        100000 : 9592,
        1000000 : 78498,
        10000000 : 664579,
        100000000 : 5761455,
        1000000000 : 50847534,
        10000000000 : 455052511
    ]
    
    
    init(size: Int) {
        sieveSize = size
        rawBits = UnsafeMutableRawBufferPointer.allocate(byteCount: size / 8 + 1, alignment: MemoryLayout<UInt8>.alignment)
        rawBits.initializeMemory(as: UInt8.self, repeating: 0xff)
    }
    
    
    deinit {
        rawBits.deallocate()
    }
    
    
    func validateResults() -> Bool {
        return myDict[sieveSize] == countPrimes()
    }
    
    
    func getBit(index: Int) -> Bool {
        
        if index % 2 == 0 {
            return false
        }
        
        return (rawBits[index / 8] & (1 << (index % 8))) != 0
    }
    
    
    func clearBit(index: Int) {
        
        if (index % 2 == 0) {
            print("You're setting even bits, which is sub-optimal.")
            return
        }
        
        rawBits[index / 8] &= ~(1 << (index % 8))
    }
    
    
    func runSieve() {
        
        var factor = 3
        let q = Int(sqrt(Double(sieveSize)))

        while (factor <= q) {
            for num in factor..<sieveSize {
                if (getBit(index: num)) {
                    factor = num
                    break
                }
            }
            for num in stride(from: factor * 3, to: sieveSize, by: factor * 2) {
                clearBit(index: num)
            }
            
            factor += 2
        }
    }
    
    func printResults(showResults: Bool, duration: TimeInterval, passes: Int) {
        
        if showResults {
            print("2, ", terminator: "")
        }
        
        var count = 1
        for num in 3..<sieveSize {
            if getBit(index: num) {
                if showResults {
                    print(String(format: "%d, ", num), terminator: "")
                }
                count += 1
            }
        }

        if showResults {
            print()
        }
        
        let output = String(format: "Passes: %d, Time: %lf, Avg: %lf, Limit: %d, Count: %d, Valid: %d",
                            passes,
                            duration,
                            duration / Double(passes),
                            sieveSize,
                            count,
                            validateResults()
        )
        print(output)
    }
    
    
    func countPrimes() -> Int {
        
        var count = 0
        for i in 0..<sieveSize {
            if getBit(index: i) {
                count += 1
            }
        }
        return count
    }
}

var passes = 0
var sieve: PrimeSieve?

let tStart = Date()
while Date().timeIntervalSince(tStart) < 5 {

    sieve = PrimeSieve(size: 1000000)
    sieve?.runSieve()
    passes += 1
}

let tD = Date().timeIntervalSince(tStart)

if let sieve = sieve {
    sieve.printResults(showResults: false, duration: tD, passes: passes)
}


