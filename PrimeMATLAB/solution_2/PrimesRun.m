%{
Written By: Brandon Johns
Date Version Created: 2021-07-04
Date Last Edited: 2021-07-16
Purpose: find prime numbers
Status: functional
Referenced files: PrimeSieve.m

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
https://au.mathworks.com/help/matlab/ref/matlabwindows.html

cd <directory containing file>
matlab -singleCompThread -batch PrimesRun(101,'all')
matlab -singleCompThread -batch PrimesRun(1000000,'stats')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Notes
%%%%%%%%%%%%%%%%%%%%%%%%%%%
Profiling tool
    run command: profile viewer

Compare to inbuilt
    open primes
        Uses a very similar method

%}
function PrimesRun(sieveSize, output)
arguments
    sieveSize(1,1) double {mustBePositive(sieveSize), mustBeInteger(sieveSize)} = 1000000
    output(1,1) string {mustBeMember(output, ["basic","stats","all"])} = "basic"
end
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