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
        rawBits.initializeMemory(as: UInt8.self, repeating: 0xaa)
    }
    
    
    deinit {
        rawBits.deallocate()
    }
    
    
    func validateResults() -> Bool {
        return myDict[sieveSize] == countPrimes()
    }
    
    
    func getBit(index: Int) -> Bool {
        return (rawBits[index / 8] & (1 << (index % 8))) != 0
    }
    
    
    func clearBit(index: Int) {
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
    
    func printResults(showResults: Bool, duration: TimeInterval, passes: Int64) {
        
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
        
        let output = String(format: "Passes: %lld, Time: %lf, Avg: %lf, Limit: %d, Count: %d, Valid: %d",
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

var passes: Int64 = 0
var sieves = [PrimeSieve]()

let tStart = Date()
while Date().timeIntervalSince(tStart) < 10 {

    let sieve = PrimeSieve(size: 100000000)
    sieves.append(sieve)
    
    DispatchQueue.global().async {
        sieve.runSieve()
        OSAtomicIncrement64(&passes)
    }
}

let tD = Date().timeIntervalSince(tStart)
sieves.first?.printResults(showResults: false, duration: tD, passes: passes)



