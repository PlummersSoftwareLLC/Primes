classdef PrimeSieve1Bit < handle
properties (Access=private)
    sieveSize
    bits
end
properties (Access=private, Constant=true)
    resultsDictionaryKey = 10.^(1:10)
    resultsDictionaryVal = [4, 25, 168, 1229, 9592, 78498, 664579, 5761455, 50847534, 455052511]
end

methods (Access=public)
    function this = PrimeSieve1Bit(n)
        % Input validation
        % Upper bound, find all primes in range [1,n]
        validateattributes(n, {"numeric"}, {"scalar", "integer", "positive", "real", "finite"})
        
        % Assign properties
        this.sieveSize = n;
        
        % Using inverted logic: 0=prime, 1=not prime
        % Exclude even numbers => bits = [1,3,5,...,n]
        % Edge case: 1 is not a prime, but 2 is
        % => must leave bit(1)=prime so that its value counts for the absense of 2
        this.bits = uint8(zeros(ceil(n/16), 1));
        
        % Clear out of range bits
        for idxClear = ceil(this.sieveSize/2)+1 : 8*length(this.bits)
            this.ClearBit(idxClear);
        end
    end
    
    function RunSieve(this)
        factor = 3;
        
        while factor <= sqrt(this.sieveSize)
            % Clear multiples of the prime
            for idxClear = ceil((factor^2)/2) : factor : ceil(this.sieveSize/2)
                this.ClearBit(idxClear);
            end
            
            % Find next prime
            %   This method is way faster than using find()
            for num = factor+2 : this.sieveSize
                if this.GetBit(ceil(num/2))
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
        
        fprintf("Brandon-Johns_1bit;");
        fprintf("%d;", passes);
        fprintf("%f;", duration);
        fprintf("1;");
        fprintf("algorithm=base,faithful=yes,bits=1\n");
    end
    
    function count = CountPrimes(this)
        count = 0;
        for idx_bit = 1:8
            % Invert on retrieval
            count = count + sum(~bitget(this.bits, idx_bit));
        end
    end
end
methods (Access=private)
    % Get/Clear Bit
    % Uses manual bit packing as there is no base bit type (logicals use 8 bits)
    % INPUT: n = position of bit in array, starting at 1 (n is not the number represented by the bit)
    function bit = GetBit(this, n)
        idx_byte = floor((n-1)/8) + 1;
        idx_bit = mod((n-1),8) + 1;
        % Invert on retrieval
        bit = ~bitget(this.bits(idx_byte), idx_bit);
    end
    
    function ClearBit(this, n)
        idx_byte = floor((n-1)/8) + 1;
        idx_bit = mod((n-1),8) + 1;
        % Invert on set
        this.bits(idx_byte) = bitset(this.bits(idx_byte), idx_bit, 1);
    end
    
    function primeArray = PrimeArray(this)
        % List all primes in an array
        primeArray = 2; % See note on edge case in constructor
        for n = 2 : ceil(this.sieveSize/2)
            if GetBit(this, n)
                primeArray(end+1) = 2*n - 1;
            end
        end
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
