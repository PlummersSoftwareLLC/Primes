import ballerina/io;
import ballerina/time;

public class PrimeSieve {
    private  boolean[] bitArray;
    private int sieveSize;

    private map<int> primeCounts = {
        "10": 4,
        "100": 25,
        "1000": 168,
        "10000": 1229,
        "100000": 9592,
        "1000000": 78498,
        "10000000": 664579,
        "100000000": 5761455 
    };

    public function init(int size) {
        self.sieveSize = size;
        self.bitArray = [];
        
        int i = 0;
        while i < ((self.sieveSize + 1) / 2) {
            self.bitArray[i] = true;
            i += 1;
        }
    }

    public function countPrimes() returns int {
        int count = 0;
        int i = 0;
        while i < self.bitArray.length() {
            if self.bitArray[i] {
                count += 1;
            }
            i += 1;
        }

        return count;
    }

    public function validateResults() returns boolean {
        if self.primeCounts.hasKey(self.sieveSize.toString()) {
            return self.primeCounts[self.sieveSize.toString()] == self.countPrimes();
        }
        return false;
    }

    private function GetBit(int index) returns boolean {
        if index % 2 == 0 {
            return false;
        }

        return self.bitArray[index / 2];
    }

    private function ClearBit(int index) {
        if index % 2 == 0 {
            io:println("You are setting even bits, which is sub-optimal");
            return;
        }
        self.bitArray[index / 2] = false;
    }

    // Calculate the primes up to the specified limit
    public function runSieve() {
        int factor = 3;
        int q = <int>float:sqrt(<float>self.sieveSize);

        while factor < q {
            int num = factor;
            while num <= self.sieveSize {
                if self.GetBit(num) {
                    factor = num;
                    break;
                }
                num += 1;
            }

            num = factor * 3;
            while num <= self.sieveSize {
                self.ClearBit(num);
                num += factor * 2;
            }

            factor += 2;
        }
    }

    public function printResults(boolean showResults, int duration, int passes) {
        if showResults {
            io:print("2, ");
        }

        int count = 1;
        int num = 3;
        while num <= self.sieveSize {
            if self.GetBit(num) {
                if showResults {
                    io:print(num.toString() + ", ");
                }
                count += 1;
            }
            num += 1;
        }
        if showResults {
            io:println("");
        }

        float average = <float>duration / <float>passes;
        io:println(`Passes: ${passes}, Time: ${duration}, Avg: ${average}, Limit: ${self.sieveSize}, Count: ${count}, Valid: ${self.validateResults()}`);

        io:println("");
        io:println(`da-strange-boi;${passes};${duration};1;algorithm=base,faithful=yes,parallel=no,bits=1`);
    }
}

public function main() {
    int tStart = time:utcNow()[0];
    int passes = 0;
    PrimeSieve sieve;

    while time:utcNow()[0] - tStart < 5 {
        sieve = new PrimeSieve(1000000);
        sieve.runSieve();
        passes += 1;
    }

    int tD = time:utcNow()[0] - tStart;
    sieve.printResults(false, <int>tD, passes);
}
