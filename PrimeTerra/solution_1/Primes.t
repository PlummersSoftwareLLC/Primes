-- Primes.t
local c = terralib.includecstring[[
    #include <stdio.h>
    #include <stdlib.h>
    #include <time.h>
]]

import "macro" -- simplifiyng macro syntax to stop using unnecessary macro(function()) bullshit

local N = 1000000
local st = math.floor(math.sqrt(N))
local dur = 5

struct Primes{
    finish: bool; 
    cur: uint64; -- iterator
    check: uint64; -- iterator
    prime: uint64; -- iterator
    res: uint64; -- the amount of primes goes here
    size: uint64;
    arr: &uint64;
    nums: &uint64;
};

macro Primes:new()
    return quote
        self.arr = [&uint64](c.malloc(sizeof(uint64)*N/2))
        var j = 0
        for i = 1, N, 2 do
            self.arr[j] = i
            j = j + 1 
        end
        self.size = N/2
        self.arr[0] = 2
        self.check = 1
        self.prime = 1
        self.res = 0
        self.cur = 1
        self.nums = nil
        self.finish = false
    end
end

macro Primes:sort()
    return quote
        
        var last = self.check + 1
        var check = self.arr[self.check]
        for i = last, self.size do
            if self.arr[i] % check ~= 0 then
                self.arr[i], self.arr[last] = self.arr[last], self.arr[i]
                last = last + 1
                
            end
        end
        self.check = self.check + 1
        
    end
end


macro Primes:fillin_gaps()
    return quote
        var last = self.prime + 1
        var a = self.arr
        var check = a[self.prime]
        for i = last, self.size do
            if a[i] == 1 then 
                i = i + 1
            end
            if a[i] % check == 0 then
                a[i] = 1
                
            end
        end
        self.prime = last
    end
end

macro Primes:get_result()
    return quote
        var i = self.cur
        if i < self.size then
            if self.arr[i] == 1 then
                self.res = i
                self.finish = true
            end
            self.res = i
            i = i + 1
        end
        
        self.cur = i
    end
end

macro Primes:save_sieve()
    return quote
        self.nums = [&uint64](c.malloc(self.res*sizeof(uint64)))
        for i = 0, self.res do
            self.nums[i] = self.arr[i]
        end
    end
end

macro Primes:print() -- macro for debugging
    return quote
        for i = 0, N/2 do
            c.printf("%d ", self.arr[i])
        end
        c.printf("\n")
    end
end


terra main()
    var pass = 1
    
    var p: Primes

    p:new()
    var start = c.clock()
    var ct = c.clock() - start
    
    while true do
        if p.check < st then
            p:sort() -- sorting values
        elseif p.prime < 2 then -- yea, i don't need much for this
            p:fillin_gaps() -- filling non-prime values
        else
            if p.finish then
                p:save_sieve() -- saving results
                ct = c.clock() - start
                break;
            end
            p:get_result() -- finding first non-prime - easy
        end
        ct = c.clock() - start
        pass = pass + 1
        if ct/1000 >= dur then
           
            break;
        end
    end
    c.printf("Computing primes to 1000000 on 1 thread for 5 seconds.\n\
 Passes: %d, Time: %f, Avg: %f, Limit: %d, Count: %d, Valid: %d\n", pass, ct*0.001, 0.0, N, p.res, 64)
    c.printf("Enter1he;%d;%f;64;algorithm=other,faithful=no,bits=1", pass, ct*0.001)
    c.free(p.arr)
    if p.nums ~= nil then
        c.free(p.nums)
    end
    
end


terralib.saveobj("Primes.exe", {main=main}, true)