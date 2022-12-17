-- Primes.t
local c = terralib.includecstring[[
    #include <stdio.h>
    #include <stdlib.h>
    #include <time.h>
    #include <string.h>
]]

local std = require "std"

import "macro" -- simplifiyng macro syntax to stop using unnecessary macro(function()) bullshit

local N = 1000000
local st = math.ceil(math.sqrt(N))
local clocks = 0.000001
local dur = 5

struct Primes{
    res: uint32; -- the amount of primes goes here
    size: uint32;
    arr: &uint32;
};

macro Primes:new()
    return quote
        var size = N >> 5
        
        if N % 32 > 0 then
            size = size + 1
        end
        self.arr = [&uint32](c.malloc(sizeof(uint32)*(N>>5)))
        
        self.arr[0] = 0x55555556
        for i = 1, size do
            self.arr[i] = 0x55555555
        end
        self.size = size
        self.res = 1
    end
end

macro Primes:get_subOne(i)
    return `(self.arr[i >> 5] >> (i % 32)) and 0x01
end

macro Primes:set_subOne(i)
    return quote
        self.arr[i >> 5] = self.arr[i >> 5] and (not(0x01 << (i % 32)))
    end
end

macro rol(x, n) 
    return `((x<<n) or (x>>(32-n)))
end

macro Primes:run()
    return quote
        var check = 3
        while check <= st do
            if self:get_subOne(check - 1) == 1 then
                var skip = check + check
                var start = check*check
                var rolling_mask = not[uint32](1 << (start-1) % 32);
                var roll_bits = skip % 32;
                for n = start, N+1, skip do
                    self.arr[n>>5] = self.arr[n>>5] and rolling_mask
                    rolling_mask = rol(rolling_mask, roll_bits)
                end
            end
            check = check + 2
        end
    end
end

macro Primes:count()
    return quote
        self.res = 1
        for i = 2, N, 2 do
            if self:get_subOne(i) == 1 then
                self.res = self.res + 1
            end
        end
    end
end

macro Primes:print() -- macro for debugging
    return quote
        var count = 0
        for i = 0, N  do
            c.printf("%d ", self:get_subOne(i))
            count = count + 1
        end
    end
end

macro println()
    return `c.printf"\n"
end

macro Primes:draw() -- prints the values of self.arr
    return quote
        for n = 0, N do
            if self:get_subOne(n) == 1 then
                c.printf("%d ", n+1)
            end
        end
        println()
    end
end

macro Primes:printResults(printNums, pass, ct)
    return quote
        if printNums then
            self:draw()
        end
        self:count()
        var ti = ct*clocks
        c.printf("Computing primes to 1000000 on 1 thread for 5 seconds.\n\
 Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %d\n", pass, ti, ti/pass, N, self.res, self:validate())
        c.printf("Enter1he;%d;%f;1;algorithm=base,faithful=no,bits=1\n", pass, ti)
    end
end

local results = {
             [10] = 4,               -- Historical data for validating our results - the number of primes
            [100] = 25,              -- to be found under some limit, such as 168 primes under 1000
           [1000] = 168,
          [10000] = 1229,
         [100000] = 9592,
        [1000000] = 78498,
       [10000000] = 664579,
      [100000000] = 5761455,
     [1000000000] = 50847534,
    [10000000000] = 455052511,
}

terra Primes:validate() : bool
    return [ results[N] ] == self.res
end

macro Primes:delete()
return quote
    c.free(self.arr)
    end
end

terra main()
    var pass = 0
    var ct: c.clock_t = 0
    
    var start = c.clock()
    var fin = true
    while fin do
        var p: Primes
        p:new()
        

        p:run()
        pass = pass + 1

        ct = c.clock() - start
        if ct*clocks >= dur then
            p:printResults(false, pass, ct)
            fin = false
        end
        p:delete()
    end
    
end



terralib.saveobj("Sieve", {main=main}, {"-O3"}, nil, true)