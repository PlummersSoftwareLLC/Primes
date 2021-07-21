%{
Language: Octave
Version: 6.3.0 or 4.4.2 both tested to work

Written By: Brandon Johns
Date Version Created: 2021-07-17
Date Last Edited: 2021-07-17
Purpose: find prime numbers
Status: functional

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
About
%%%%%%%%%%%%%%%%%%%%%%%%%%%
Implementation for
https://github.com/PlummersSoftwareLLC/Primes/tree/drag-race

Using algorithm
https://en.wikipedia.org/wiki/Sieve_of_Eratosthenes

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Run from CLI
%%%%%%%%%%%%%%%%%%%%%%%%%%%
flatpak run org.octave.Octave

%}
function PrimesRun(mode="8bit", sieveSize=1000000, output="basic")
    % Input validation
    validatestring(mode, {"8bit", "1bit"});
    validateattributes(sieveSize, {"numeric"}, {"scalar", "integer", "positive", "real", "finite"})
    validatestring(output, {"basic","stats","all"});

    % Mode switch
    %   Bad practice, but otherwise I'd risk decreased performance

    % MODE 1: 8 bit, with vectorisation
    if strcmp(mode, "8bit")
        passes = 0;
        tic; % Start timer
        while true
            sieve = PrimeSieve(sieveSize);
            sieve.RunSieve();
            passes = passes + 1;
            if (toc >= 5)
                sieve.PrintResults(toc, passes, output);
                break;
            end
        end
    
    % MODE 2: 1 bit
    else
        passes = 0;
        tic; % Start timer
        while true
            sieve = PrimeSieve1Bit(sieveSize);
            sieve.RunSieve();
            passes = passes + 1;
            if (toc >= 5)
                sieve.PrintResults(toc, passes, output);
                break;
            end
        end
    end
end