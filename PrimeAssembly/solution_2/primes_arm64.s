// This implementation is a faithful implementation in arn64 assembly.
// It can be used for sieve sizes up to 100,000,000// beyond that some register widths used will become too narrow for
// (first) the sqrt of the size, and then the size itself.

.arch armv8-a+simd

.global main

.extern printf
.extern malloc
.extern free

                    .struct     0
time_sec:           .struct     time_sec + 8
time_fract:         .struct     time_fract + 8
time_size:

                    .struct     0
sieve_limit:        .struct     sieve_limit + 4
sieve_arraySize:    .struct     sieve_arraySize + 4
sieve_primes:       .struct     sieve_primes + 8
sieve_size:

.equ                SIEVE_SIZE,     1000000     // sieve size
.equ                RUNTIME,        5           // target run time in seconds
.equ                FALSE,          0           // false constant
.equ                NULL,           0           // null pointer
.equ                INIT_PATTERN,   0x0101      // init pattern for prime array            

.equ                CLOCK_GETTIME,  108         // syscall number for clock_gettime
.equ                CLOCK_MONOTONIC,1           // CLOCK_MONOTONIC
.equ                WRITE,          64          // syscall number for write
.equ                STDOUT,         1           // file descriptor of stdout

.equ                MILLION,        1000000
.equ                BILLION,        1000000000

.text

main:

// registers (global variables):
// * x22: billion
// * x24: sieveSize
// * w25: runCount
// * x26: sievePtr (&sieve)
// * w27: sizeSqrt
// * x28: initBlock

    movz            x28, INIT_PATTERN           // set 2 rightmost bytes of initBlock...
    movk            x28, INIT_PATTERN, lsl 16   // ...then the 2 left of that... 
    movk            x28, INIT_PATTERN, lsl 32   // ...then the 2 left of that...
    movk            x28, INIT_PATTERN, lsl 48   // ...then the 2 leftmost

    ldr             x22, =BILLION               // billion = BILLION      

    mov             w25, #0                     // runCount = 0

    ldr             x24, =SIEVE_SIZE            // sieveSize = sieve size

    ucvtf           s0, x24
    fsqrt           s0, s0                      // s0 = sqrt(s0)
    fcvtau          x27, s0                     // sizeSqrt = s0 
    add             w27, w27, #1                // sizeSqrt++, for safety 

// get start time
    mov             x8, CLOCK_GETTIME           // syscall to make, parameters:
    mov             x0, CLOCK_MONOTONIC         // * ask for monotonic time
    ldr             x1, startTime               // * struct to store result in
    svc             #0

    mov             x26, #0                     // sievePtr = null

runLoop:
    cbz             x26, createSieve            // if sievePtr == null then skip deletion
    
    mov             x0, x26                     // pass sievePtr
    bl              deleteSieve                 // delete sieve

createSieve:    
    mov             x0, x24                     // pass sieve size
    bl              newSieve                    // x0 = &sieve

    mov             x26, x0                     // sievePtr = rax

    bl              runSieve                    // run sieve

// registers: 
// * x0: numDurationSeconds
// * x1: numDurationNanoseconds/numDurationMilliseconds
// * x2: startTimePtr
// * x3: numStartTimeSeconds/numStartTimeNanoseconds
// * x22: billion
// * x23: durationPtr
// * x24: sieveSize
// * w25: runCount
// * x26: sievePtr (&sieve)

    mov             x8, CLOCK_GETTIME           // syscall to make, parameters:
    mov             x0, CLOCK_MONOTONIC         // * ask for monotonic time
    ldr             x1, duration                // * struct to store result in
    svc             #0

    ldr             x2, startTime               // startTimePtr = &startTime
    ldr             x23, duration               // durationPtr = &duration

    ldr             x0, [x23, #time_sec]        // numDurationSeconds = duration.seconds
    ldr             x3, [x2, #time_sec]         // numStartTimeseconds = starttime_seconds
    sub             x0, x0, x3                  // numDurationSeconds -= numStartTimeseconds

    ldr             x1, [x23, #time_fract]      // numDurationNanoseconds = duration.fraction
    ldr             x3, [x2, #time_fract]       // numStartTimeNanoseconds = starttime_fract
    subs            x1, x1, x3                  // numDurationNanoseconds -= numStartTimeNanoseconds

    bmi             checkTime                   // if numNanoseconds >= 0 then check the duration...
    sub             x0, x0, #1                  // ...else numSeconds--...
    add             x1, x1, x22                 // ...xw and numNanoseconds += billion

checkTime:
    add             w25, w26, #1                // runCount++
    cmp             x0, RUNTIME                 // if numSeconds < 5...
    bls             runLoop                     // ...perform another sieve run

// we're past the 5 second mark, so it's time to store the exact duration of our runs
    str             x0, [x23, #time_sec]        // duration.seconds = numSeconds

    ldr             x2, =MILLION                // x2 = 1,000,000
    udiv            x1, x1, x2                  // x1 /= x2, so x1 contains numMilliseconds

    str             x1, [x23, #time_fract]      // duration.fraction = numMilliseconds

// let's count our primes
    mov             x0, x26                     // pass sievePtr
    bl              countPrimes                 // x0 = primeCount

// registers:
// * x0: primeCount
// * x1: refResultPtr
// * x2: curSieveSize/curResult
// * x24: sieveSize
// * w25: runCount

    ldr             x1, refResults              // refResultPtr = (int *)&refResults

checkLoop:
    ldr             x2, [x1]                    // curSieveSize = *refResultPtr
    cbz             x2, printWarning            // if curSieveSize == 0 then we didn't find our sieve size, so warn about incorrect result
    cmp             x2, x24                     // if curSieveSize == sieveSize...
    beq             checkValue                  // ...check the reference result value...
    add             x1, x1, #8                  // ...else refResultsPtr += 2 
    b               checkLoop                   // keep looking for sieve size

checkValue:
    ldr             x2, [x1, #4]                // curResult = *(refResultPtr + 1)
    cmp             w2, w0                      // if curResult == primeCount... 
    beq             printResults                // ...print result

// if we're here, something's amiss with our outcome
printWarning:
    mov             x8, WRITE                   // syscall to make, parameters:
    mov             x0, STDOUT                  // * write to stdout
    ldr             x1, incorrect               // * message is warning
    mov             x2, incorrectLen            // * length of message
    svc             #0

printResults:
                                                // parameters for call to printf:
    ldr             x0, outputFmt               // * format string
    mov             w1, w25                     // * runCount
    ldr             x2, [x23, #time_sec]        // * duration.seconds
    ldr             x3, [x23, #time_fract]      // * duration.fraction (milliseconds)
    bl              printf                             

    mov             x0, #0                      // return 0

    ret                                         // end of main

// parameters:
// * x0: sieve size
// returns:
// * x0: &sieve
newSieve:

// registers:
// * x20 = sievePtr (&sieve)

    mov             x19, x0                     // keep parameter, we'll need it later

    mov             x0, #sieve_size             // ask for sieve_size bytes
    bl              malloc                      // x0 = &sieve

    mov             x20, x0                     // sievePtr = x0

    str             w19, [x0, #sieve_limit]     // sieve_limit = sieve size (limit)
    lsr             w19, w19, #1                // array_size = sieve_limit / 2
    add             w19, w19, #1                // array_size++
    str             w19, [x0, #sieve_arraySize] // sieve_arraySize = array_size

// registers:
// * x0 = initBlockBytes
// * x1 = initBlockIndex
// * x2 = init_block
// * w19 = initBlockCount
// * x20 = sievePtr (&sieve)
// * x28 = initBlock

    lsr             w19, w19, #3                // initBlockCount /= 8
    add             w19, w19, #1                // initBlockCount++
    
    mov             x0, #0                      // initBlockBytes = 0
    ldr             w0, w19                     // initBlockBytes = initBlockCount
    lsl             w0, w0, #3                  // initBlockBytes *= 8
    bl              malloc                      // x0 = &array[0]

    str             x0, [x20, #sieve_primes]    // sieve_primes = x0

// initialize prime array   
    mov             x1, #0                      // initBlockIndex = 0                       

initLoop:
    str             x28, [x0, x1, lsl 3]        // sieve_primes[initBlockIndex*8..(initBlockIndex*8 + 7)] = true
    add             x1, x1, #1                  // initBlockIndex++
    cmp             w1, w19                     // if initBlockIndex < initBlockCount...
    bls             initLoop                    // ...continue initialization

    ret                                         // end of newSieve

// parameters:
// * x0: sievePtr (&sieve)
deleteSieve:
    mov             x19, x0                     // keep sievePtr, we'll need it later

    ldr             x0, [x19, #sieve_primes]    // ask to free sieve_primes
    bl              free

    mov             x0, x19                     // ask to free sieve
    bl              free

    ret                                         // end of deleteSieve

// parameters:
// * x0: sievePtr (&sieve)
// returns:
// * &sieve_primes[0]
runSieve:

// registers:
// * x1: primesPtr (&sieve_primes[0])
// * x2: number
// * x3: factor
// * x4: arrayIndex
// * w5: false
// * x6: 2
// * x7: sieveLimit
// * w8: curPrime (sieve_primes[arrayIndex])
// * w27: sizeSqrt (global)

    ldr             x1, [x0, #sieve_primes]     // primesPtr = &sieve_primes[0]
    mov             x3, #3                      // factor = 3
    mov             x4, #0                      // arrayIndex = 0
    mov             w5, FALSE                   // false = FALSE
    mov             x6, #2                      // w6 = 2
    ldr             x7, [x0, #sieve_limit]      // sieveLimit = sieve_limit   

sieveLoop:
    mul             x2, x3, x3                  // number = factor * factor

// clear multiples of factor
unsetLoop:
    mov             x4, x2                      // arrayIndex = number                         
    lsr             x4, x4, #1                  // arrayIndex /= 2

    strb            w5, [x1, x4]    	        // sieve_primes[arrayIndex] = false
    madd            x2, x3, x6, x2              // number += 2*factor
    cmp             x2, x7                      // if number <= sieveLimit...
    bls             unsetLoop                   // ...continue marking non-primes

// find next factor
factorLoop:
    add             x3, x3, #2                  // factor += 2
    cmp             w3, w27                     // if factor > sizeSqrt...
    bhi             endRun                      // ...end this run
    
    mov             x4, x3                      // arrayIndex = factor
    lsr             x4, x4, #1                  // arrayIndex /= 2

    ldrb            w8, [x1, x4]                // curPrime = sieve_primes[arrayIndex]
    cbnz            w8, sieveLoop               // if curPrime then continue run
    b               factorLoop                  // continue looking

endRun:
    mov             x0, x1                      // return &sieve_primes[0]

    ret                                         // end of runSieve


// parameters:
// * x0: sievePtr (&sieve)
// returns:
// * primeCount
countPrimes:

// registers:
// * x1: arraySize
// * x2: primesPtr (&sieve_primes[0])
// * w3: primeCount
// * x4: arrayIndex
// * w5: curPrime (sieve_primes[arrayIndex])

    ldr             x1, [x0, #sieve_arraySize]  // arraySize = sieve_arraySize
    ldr             x2, [x0, #sieve_primes]     // primesPtr = &sieve_primes[0]
    mov             w3, #0                      // primeCount = 0
    mov             x4, #2                      // arrayIndex = 2

countLoop:    
    ldrb            w5, [x1, x3]                // curPrime = sieve_primes[arrayIndex]
    cmp             w5, FALSE                   // if !curPrime...
    cinc            w3, w3, eq                  // ...primeCount++
    add             x4, x4, #1                  // arrayIndex++
    cmp             x4, x1                      // if arrayIndex <= arraySize...
    bls             countLoop                   // ...continue counting

    ret                                         // end of countPrimes

.align              4

outputFmt:                                      // format string for output
.asciz              "rbergen_arm64//%d//%d.%03d//1\n"   

.align              4

incorrect:                                      // incorrect result warning message
.asciz              "WARNING: result is incorrect!\n"

.equ                incorrectLen, . - incorrect // length of previous

.data

.align              4

refResults:
.word               10, 4
.word               100, 25
.word               1000, 168
.word               10000, 1229
.word               100000, 9592
.word               1000000, 78498
.word               10000000, 664579
.word               100000000, 5761455
.word               0

.align              4

startTime:                              // start time of sieve run
.skip               time_size                           

.align              4

duration:                               // duration
.skip               time_size                           
