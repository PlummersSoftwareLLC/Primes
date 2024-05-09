-- Primes.t
local c = terralib.includecstring[[
    #include <stdio.h>
    #include <stdlib.h>
    #include <time.h>
    #include <string.h>
]]

local ffi = require"ffi"

terralib.os = ffi.os


import "macro" -- simplifiyng macro syntax to stop using unnecessary macro(function()) bullshit

local N = 1000000
local st = math.ceil(math.sqrt(N))
local progname = "Sieve2"
local clocks = 0.001
if terralib.os == "Windows" then
    clocks = 0.001
    progname = "Sieve2.exe"
else
    clocks = 0.000001
end
local dur = 5

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


function Primes(N)
    local arr = terralib.new(int8[N])
    local arrsize = N
    for i = 0, arrsize-1 do 
        arr[i] = 1
    end
    arr[0] = 0
    for i = 2, arrsize-1 do
        if i % 2 == 0 then
            arr[i] = 0
        end
    end
    local check = 3
    while check <= st do
        
        local skip = check+check
        local start = check*check
        for i = start, N+1, skip do
            arr[i] = 0
        end
        check = check + 2
    end

    local ret = terralib.constant(arr)
    
    local struct Primes{
        res: uint32; -- the amount of primes goes here
        size: uint32;
        arr : int8[N];
    }
    
    terra Primes:new()
        self.arr = ret
        return arr
    end

    terra Primes:count()
        var count = 0
        for i = 0, arrsize-1 do 
            count = count + self.arr[i]
        end
        self.res = count
    end

    terra Primes:print()
        for i = 0, arrsize-1 do
            c.printf("%d", self.arr[i])
        end
    end

    macro Primes:printResults(printNums, pass, ct)
        return quote
            
            self:count()
            var ti = ct*clocks
            c.printf("Computing primes to 1000000 on 1 thread for 5 seconds.\n\
Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %d\n", pass, ti, ti/pass, N, self.res, self:validate())
            c.printf("Enter1he;%d;%f;1;algorithm=other,faithful=no,bits=8\n", pass, ti)
        end
    end
    macro Primes:validate()
        return `terralib.select([ results[N] ] == self.res, 1, 0)
    end

    return Primes
end




terra main()
    var pass = 0
    var ct: c.clock_t = 0
    
    var start = c.clock()
    var fin = true
    while fin do
        var p : Primes(N)
        
        p:new()
        
        pass = pass + 1

        ct = c.clock() - start
        if ct*clocks >= dur then
            p:printResults(false, pass, ct)
            fin = false
        end
    end
    
end



terralib.saveobj(progname, {main=main}, nil, nil, {fastmath = true})