classdef PrimeSieve < handle
properties (Access=private)
    sieveSize
    bits
end
properties (Access=private, Constant=true)
    resultsDictionaryKey = 10.^(1:10)
    resultsDictionaryVal = [4, 25, 168, 1229, 9592, 78498, 664579, 5761455, 50847534, 455052511]
end

methods (Access=public)
    function this = PrimeSieve(n=1000000)
        % Input validation
        % Upper bound, find all primes in range [1,n]
        validateattributes(n, {"numeric"}, {"scalar", "integer", "positive", "real", "finite"})
        
        % Assign properties
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
            this.bits(idxClear) = false;
            
            % Find next prime
            %   This method is way faster than using find()
            for num = factor+2 : 2 : this.sieveSize
                if this.bits(ceil(num/2))
                    factor = num;
                    break;
                end
            end
        end
    end
    
    function PrintResults(this, duration, passes, output="basic")
        % Input validation
        % Total duration of trials [seconds]
        validateattributes(duration, {"numeric"}, {"scalar", "positive", "real", "finite"})
        % Number of trials [count]
        validateattributes(passes, {"numeric"}, {"scalar", "integer", "positive", "real", "finite"})
        % Output options
        validatestring(output, {"basic","stats","all"});
        
        if strcmp(output,"all")
            % Print all primes
            fprintf("%d,", this.PrimeArray);
            fprintf("\n\n");
        end
        
        if ~strcmp(output,"basic")
            fprintf("Passes: %d, ", passes);
            fprintf("Time: %f, ", duration);
            fprintf("Avg: %f, ", duration/passes);
            fprintf("Limit: %d, ", this.sieveSize);
            fprintf("Count: %d, ", this.CountPrimes);
            fprintf("Valid: %d\n\n", this.ValidateResults);
        end
        
        % To test the number of bits used by a logical value, run: a=true(1,9);whos("a")
        % This shows 9 bytes used for a array of 9 logicals
        % => Each logical uses 1 byte
        fprintf("Brandon-Johns_8bit;");
        fprintf("%d;", passes);
        fprintf("%f;", duration);
        fprintf("1;");
        fprintf("algorithm=base,faithful=yes,bits=8\n");
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
        idx_key = find(this.resultsDictionaryKey==this.sieveSize);
        if idx_key
            % In dictionary
            isValid = this.resultsDictionaryVal(idx_key) == this.CountPrimes;
        else
            % Not in dictionary => Check with inbuilt function
            primeArray = this.PrimeArray;
            isValid = all(primes(this.sieveSize)==primeArray);
        end
    end
end
end
