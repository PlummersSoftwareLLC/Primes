class SieveClass {
    bitArray = null;
    sieveSize = null;
        constructor(maxSize) {
            bitArray = array(maxSize);
            sieveSize = maxSize;
        }

    function GetBit(index)
    {
        return bitArray[index];
    }

    function runSieve()
    {
        local factor = 3;
        local sq = sqrt(sieveSize);

        while (factor <= sq) {
            for (local i = factor; i < sieveSize; i += 2) {
                if (GetBit(i) == null) {
                    factor = i;
                    break;
                }
            }
			local factordoubled = factor * 2;
            for (local num = factor * factor; num < sieveSize; num += factordoubled)
            {
                bitArray[num] = 1; 			// ClearBit(num);
            }
            factor += 2;
        }   
    }

    function writeResults(end, passes)
    {
        local number = 1; // account for the prime number 2
        foreach (i, j in bitArray) {
            if (i > 2 && i % 2 != 0 && bitArray[i] == null) {
                number++;
            }
        }
		if (number == 78498) {
			print("Tmob;" + passes + ";" + end + ";" + "1;algorithm=base,faithful=yes\n");
		}
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
        passes++;
    }
    local progEnd = clock();

    sieve.writeResults(progEnd, passes);
}


main();