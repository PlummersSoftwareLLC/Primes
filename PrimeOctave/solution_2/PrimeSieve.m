classdef PrimeSieve < handle
properties (Access=private)
    sieveSize(1,1) double {mustBePositive(sieveSize), mustBeInteger(sieveSize)} = 1000000
    bits(1,:) logical
end
properties (Access=private, Constant=true)
    resultsDictionary = containers.Map(...
        10.^(1:10),...
        [4, 25, 168, 1229, 9592, 78498, 664579, 5761455, 50847534, 455052511])
end

methods (Access=public)
    function this = PrimeSieve(n)
        this.sieveSize = n;
        
        % Exclude even numbers => Bits = [1,3,5,...,n]
        % Edge case: 1 is not a prime, but 2 is
        % => must leave 1=true so that its value counts for the absense of 2
        this.bits = true(ceil(n/2), 1);
    end
    
    function RunSieve(this)
        factor = 3;
        
        while factor <= sqrt(this.sieveSize)
            % Clear multiples of the prime
            idxClear = ceil((factor^2)/2) : factor : ceil(this.sieveSize/2);
            this.bits(idxClear) = false; % <- the bottleneck. not much more I can do :(
            
            % Find next prime
            %   This method is way faster than using find()
            for num = factor+2 : this.sieveSize
                if this.bits(ceil(num/2))
                    factor = num;
                    break;
                end
            end
        end
    end
    
    function PrintResults(this, duration, passes, output)
        arguments
            this
            duration(1,1) double {mustBePositive(duration)} % Total duration of trials [seconds]
            passes(1,1) double {mustBePositive(passes)} % Number of trials [count]
            output(1,1) string {mustBeMember(output, ["basic","stats","all"])} = "basic"
        end
        
        if strcmp(output,"all")
            % Print all primes
            fprintf(strjoin(compose("%d", this.PrimeArray), ",") + "\n\n");
        end
        
        if ~strcmp(output,"basic")
            fprintf(...
                compose("Passes: %d, ", passes) +...
                compose("Time: %f, ", duration) +...
                compose("Avg: %f, ", duration/passes) +...
                compose("Limit: %d, ", this.sieveSize) +...
                compose("Count: %d, ", this.CountPrimes) +...
                compose("Valid: %d\n\n", this.ValidateResults)...
                );
        end
        
        % To test the number of bits used by a logical value, run: a=true(1,9);whos("a")
        % This shows 9 bytes used for a array of 9 logicals
        % => Each logical uses 1 byte
        fprintf(...
            "Brandon-Johns_8bit;" +...
            compose("%d;", passes) +...
            compose("%f;", duration) +...
            "1;" +...
            "algorithm=base,faithful=yes,bits=8\n"...
            );
    end
    
    function count = CountPrimes(this)
        count = sum(this.bits);
    end
end
methods (Access=private)
    function primeArray = PrimeArray(this)
        % List all primes in an array
        primeArray = 1 : 2 : this.sieveSize;
        primeArray = primeArray(this.bits);
        primeArray(1) = 2; % See note on edge case in constructor
    end
    
    function isValid = ValidateResults(this)
        if isKey(this.resultsDictionary, this.sieveSize)
            isValid = this.resultsDictionary(this.sieveSize) == this.CountPrimes;
        else
            % Not in dictionary => Check with inbuilt function
            primeArray = this.PrimeArray;
            isValid = all(primes(this.sieveSize)==primeArray);
        end
    end
end
end
