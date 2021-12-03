# Performance Of Code Optimizations


## While vs For

The main run loop can be constructed inside a `while` structure, or purely as `for` loops.  EG:

        for (int f = factor; f <= q; f += 2)
        {
            if (GetBit(f))
            {
                int increment = f * 2;

                for (int num = f * f; num <= SieveSize; num += increment)
                {
                    ClearBit(num);
                }
            }
        }


        while (factor <= q)
        {
            for (int num = factor; num <= q; num += 2)
            {
                if (GetBit(num))
                {
                    factor = num;
                    break;
                }
            }

            int increment = factor * 2;

            for (int num = factor * factor; num <= SieveSize; num += increment)
            {
                ClearBit(num);
            }

            factor += 2;
        }


The relative performance depends a great deal on whether this is run as a single-threaded process, or a multi-threaded process.

Bitarray for loop is 2.5% faster when run multithreaded, but 1% slower when single threaded, when using the for loop.

Bool arrays range from 1% to 14% faster using the while loop while single threaded, but up to 40% faster when using the for loop when multithreaded.

