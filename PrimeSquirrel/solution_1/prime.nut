class SieveClass {
    bitArray = null;
    sieveSize = null;
        constructor(maxSize) {
            bitArray = array(maxSize);
            sieveSize = maxSize;
            foreach (i, j in bitArray) {
                bitArray[i] = 1;
            }
        }

    function GetBit(index)
    {
        return bitArray[index];
    }

    function ClearBit(index)
    {
        bitArray[index] = 0;
    }

    function runSieve()
    {
        ClearBit(0);
        ClearBit(1);
    
        local factor = 3;
        local q = sqrt(sieveSize);

        while (factor <= q) {
            for (local i = factor; i < sieveSize; i += 2) {
                if (GetBit(i) == 1) {
                    factor = i;
                    break;
                }
            }

            for (local num = factor * factor; num < sieveSize; num += factor * 2)
            {
                ClearBit(num);
            }
            factor += 2;
        }   
    }

    function writeResults(end, passes)
    {
        local number = 1; // account for the prime number 2
        foreach (i, j in bitArray) {
            if (i > 2 && i % 2 != 0 && bitArray[i] == 1) {
                number++;
            }
        }
        local finaltime = 
        print("Tmob;" + passes + ";" + end + ";" + "1;algorithm=base,faithful=yes");
    }

}


function main () {
    local passes = 0;
    local startTime = time();
    local lastTime = startTime;
    while (lastTime == startTime) // Make sure we start exact on the next sec, as time() is only precise to the sec.
    {
        lastTime = time();
    }
    local endTime = lastTime + 5;

    while (time() < endTime)
    {
        sieve <- SieveClass(1000000);
        sieve.runSieve();
        passes++
    }
    
    local progEnd = clock();

    sieve.writeResults(progEnd, passes);
}