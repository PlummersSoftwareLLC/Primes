/*
 * Drag-race entry for pure C version of prime sieve.
 *
 * Notes:
 *  No prior knowledge of primes other than 2. No clever logic in countPrimes.
 *
 * July 2021
 * by Nicholas Merriam (nicholas@merriam.me.uk)
 *
 * Acknowledgements:
 *
 * @danielspaangberg - For the gcc optimization flags
 * Colin Runciman, see https://eprints.whiterose.ac.uk/3784/1/runcimanc1.pdf
 *
 */

#include <assert.h>  // assert
#include <stdint.h>  // uint32_t
#include <stdio.h>   // printf
#include <stdlib.h>  // calloc
#include <time.h>    // clock, CLOCKS_PER_SEC
#include <string.h>  // strcmp

#include "prime-check.h"

#define TRUE (1)
#define FALSE (0)
#define BOOL int
#define BYTE unsigned char

#if defined __x86_64__
#   define WORD uint64_t
#   define BITS_PER_WORD (64)
#   define firstSetBit( word_ ) ( BITS_PER_WORD - 1 - __builtin_clzll( (WORD)(word_) ) )
#   define leastSetBit( word_ ) (                     __builtin_ctzll( (WORD)(word_) ) )
#else /* 32-bit */
#   define WORD uint32_t
#   define BITS_PER_WORD (32)
#   define firstSetBit( word_ ) ( BITS_PER_WORD - 1 - __builtin_clz( (WORD)(word_) ) )
#   define leastSetBit( word_ ) (                     __builtin_ctz( (WORD)(word_) ) )
#endif

#define wordIndexOf(n)  ( ((n) / 2) / BITS_PER_WORD )
#define bitIndexOf(n)   ( ((n) / 2) % BITS_PER_WORD )
#define maskOf(n)       ( (WORD)1u << bitIndexOf(n) )
#define allocOf(n)      ( wordIndexOf( (n)-1 ) + 1 ) // rounded up division

#define primeOf( bitIndex_ ) ( (bitIndex_) * 2 + 1 )

#define bitIsSet( buffer_, bitIndex_ ) ( (buffer_)[wordIndexOf(bitIndex_)] &   maskOf(bitIndex_) )
#define clearBit( buffer_, bitIndex_ ) ( (buffer_)[wordIndexOf(bitIndex_)] &= ~maskOf(bitIndex_) )
#define setBit(   buffer_, bitIndex_ ) ( (buffer_)[wordIndexOf(bitIndex_)] |=  maskOf(bitIndex_) )


#define DEFAULT_SIZE (1000 * 1000)

void memCpy( void *pDest, void const *pSrc, unsigned int bytes )
{
    if( 15 < bytes )
    {
        WORD *pDestWord = pDest;
        WORD const *pSrcWord = pSrc;
        unsigned int words = bytes / sizeof(WORD);
        bytes -= words * sizeof(WORD);

        while( words-- )
        {
            *pDestWord++ = *pSrcWord++;
        }
        pDest = pDestWord;
        pSrc = pSrcWord;
    }

    BYTE *pDestByte = pDest;
    BYTE const *pSrcByte = pSrc;
    while( bytes-- )
    {
        *pDestByte++ = *pSrcByte++;
    }
}

unsigned int maxNumber = DEFAULT_SIZE;

WORD const ones = (WORD)0xFFFFFFFFFFFFFFFFuLL;

typedef struct {
    unsigned int maxNumber;
    WORD *buffer;
} SIEVE;

// Search the bit array for the next set bit, which corresponds to the next prime
unsigned int nextPrime( WORD const *buffer, unsigned int lastPrime )
{
    unsigned int const primeCandidate = lastPrime + 2;

    unsigned int wordIndex = wordIndexOf( primeCandidate );
    WORD word = buffer[wordIndex];
    // Mask off lower values
    word &= ones << bitIndexOf( primeCandidate );
    // Loop termination requires that there are greater primes to be found
    while( 0uLL == word )
    {
        word = buffer[++wordIndex];
    }

    return primeOf( wordIndex * BITS_PER_WORD + leastSetBit( word ) );
}

// Remove multiples of the prime found by nextPrime
// Note that it is sufficient to remove multiples of the factors corresponding to
// remaining bits, so we search for set bits and map from bits to factors.
// Search from highest possible set bit to lowest in order that we do not remove a
// factor that we still need to use.
void sieveOnePrime( WORD *buffer, unsigned int prime, unsigned int maxNumber )
{
    // Factor to use in eliminating a multiple
    unsigned int const maxFactor = maxNumber / prime;
    // Use a signed type so that it can go below zero without underflow
    int wordIndex = wordIndexOf( maxFactor );
    while( (int)wordIndexOf( prime ) <= wordIndex )
    {
        WORD word = buffer[wordIndex];
        while( 0u != word )
        {
            unsigned int bitIndex = firstSetBit( word );
            WORD const mask = (WORD)1uLL << bitIndex;
            unsigned int factor = bitIndex * 2 + 1 + wordIndex * BITS_PER_WORD * 2;
            if( factor < prime ) break;
            word &= ~mask;
            if( maxFactor < factor ) continue;
            clearBit( buffer, factor * prime );
        }
        --wordIndex;
    }

#if defined DEBUG
    printf( "%u\n", prime );
    printf( "2 " );
    for( unsigned int i = 3u; i < maxNumber; i += 2u )
    {
        if( bitIsSet( buffer, i ) ) printf( "%u ", i );
    }
    printf( "\n" );
#endif /* defined DEBUG */
}

// maxNumber - find all primes strictly LESS than this number.
//
void primes( SIEVE *pSieve )
{
    unsigned int i; // Loop index
    unsigned int prime           = 3u;
    unsigned int primesProduct   = 1u; // Not including 2

    unsigned int const maxNumber = pSieve->maxNumber;
    unsigned int const wordMax   = allocOf( maxNumber );
    unsigned int const byteMax   = sizeof(WORD) * wordMax;
    WORD * const buffer          = pSieve->buffer;
    BYTE * const bufferByte      = (BYTE *)buffer;

    buffer[0]                    = ones; // Actually, only one byte is required
    WORD smallPrimes             = 0u;

    // Dynamic (or lazy) sieve wheel avoids reliance on pre-computed primes
    for( ;; )
    {
        // Make prime-1 more copies of initialized bytes until buffer is full
        unsigned int const bytesToCopy = primesProduct * (prime-1);
        if( byteMax < bytesToCopy + primesProduct )
        {
            // bytesToCopy would go off the end of the array
            // Just copy to byteMax and stop
            memCpy( bufferByte + primesProduct, bufferByte, byteMax - primesProduct );
            break;
        }
        // Exploit simplistic memcpy to create all required copies with one call
        memCpy( bufferByte + primesProduct, bufferByte, bytesToCopy );
        primesProduct *= prime;
        sieveOnePrime( buffer, prime, primeOf( primesProduct * 8 ) );
        // Remove these small primes from buffer and save in smallPrimes
        clearBit( buffer, prime );
        setBit( &smallPrimes, prime );
        prime = nextPrime( buffer, prime );
    }
    // Restore small primes to buffer
    buffer[0] |= smallPrimes;

    unsigned int lastWordBits = bitIndexOf( maxNumber );
    if( 0u != lastWordBits )
    {
        // Clear trailing bits outside range
        buffer[wordMax-1] &= ones >> (BITS_PER_WORD - lastWordBits);
    }

#if defined DEBUG
    printf( "%u\n", prime );
#endif /* defined DEBUG */

    // Process the remaining bits
    do
    {
        sieveOnePrime( buffer, prime, maxNumber );
        prime = nextPrime( buffer, prime );
    }
    while( prime * prime < maxNumber );

    // One is not prime
    buffer[0] &= ones << 1;

#if defined DEBUG
    printf( "2 " );
    for( i = 3u; i < maxNumber; i += 2u )
    {
        if( bitIsSet( buffer, i ) ) printf( "%u ", i );
    }
    printf( "\n" );
#endif /* defined DEBUG */
}

// Count all set bits in pSieve->buffer
unsigned int countPrimes(SIEVE const *pSieve)
{
    WORD const *buffer = pSieve->buffer;
    unsigned int maxNumber = pSieve->maxNumber;

    unsigned int count = 1u; // Two is prime
    unsigned int wordIndex = 0u;
    for( wordIndex = 0; wordIndex < allocOf(maxNumber); ++wordIndex )
    {
        count += __builtin_popcountll( buffer[wordIndex] );
    }

    return count;
}

void timedTest( int secs, void primeFinder( SIEVE *pSieve ), char *title )
{
    clock_t startTicks = clock( );
    clock_t currentTicks;

    // One pass confirms accuracy.
    int passes = 1;

    clock_t limitTicks = secs * CLOCKS_PER_SEC + startTicks;

    // Allocate sieve "object" once
    SIEVE *pSieve = malloc( sizeof(SIEVE) );
    pSieve->maxNumber = maxNumber;
    pSieve->buffer = malloc( allocOf( maxNumber ) * sizeof(WORD) );

    // Check first for accuracy against known values.
    primeFinder( pSieve );
    unsigned int primeCount = countPrimes( pSieve );
    free( pSieve->buffer );
    free( pSieve );

#if defined DEBUG
    printf("Found %d primes up to %d.\n\n", primeCount, maxNumber);
#endif /* defined DEBUG */

    for (int i = 0; i < NUM_CHECKS; i++) {
        if (primeChecks[i].n == maxNumber) {
            if (primeCount != primeChecks[i].piN) {
                printf( "%s Expected %d primes - but found %d!\n",
                        title,
                        primeChecks[i].piN,
                        primeCount );
                assert( FALSE );
            }
        }
    }

    fflush( stdout );

    do
    {
        currentTicks = clock();
        if (currentTicks >= limitTicks) {
            break;
        }
        passes++;
        pSieve = malloc( sizeof(SIEVE) );
        pSieve->maxNumber = maxNumber;
        pSieve->buffer = malloc( allocOf( maxNumber ) * sizeof(WORD) );
        primeFinder( pSieve );
        free( pSieve->buffer );
        free( pSieve );
    }
    while
#if defined DEBUG
            (0);
#else
            (1);
#endif

    float elapsed = (float) currentTicks - startTicks;

    printf("%s;%d;%0.1f;1;algorithm=wheel,faithful=yes,bits=1\n", title, passes, elapsed / CLOCKS_PER_SEC);
    fflush(stdout);
}

void usage(char *name) {
    fprintf(stderr, "Usage: %s --help --secs <s> --size <N> --debug\n", name);
    exit(1);
}

int main(int argc, char *argv[]) {
    int secs = 5;

    for (int iarg = 1; iarg < argc; iarg++) {
        if (strcmp(argv[iarg], "--help") == 0) {
            usage(argv[0]);
        } else if (strcmp(argv[iarg], "--secs") == 0) {
            if (iarg + 1 == argc) {
                fprintf(stderr,
                        "Error: Missing number of seconds to measure.\n");
                usage(argv[0]);
            }
            iarg++;
            if (sscanf(argv[iarg], "%d", &secs) != 1 || secs < 1) {
                fprintf(stderr, "Error: Invalid measurement time: %s\n",
                        argv[iarg]);
                usage(argv[0]);
            }
        } else if (strcmp(argv[iarg], "--size") == 0) {
            if (iarg + 1 == argc) {
                fprintf(stderr,
                        "Error: Missing size (range) of sieve.\n");
                usage(argv[0]);
            }
            iarg++;
            if (sscanf(argv[iarg], "%d", &maxNumber) != 1 || maxNumber < 100) {
                fprintf(stderr, "Error: Invalid sieve size: %s\n",
                        argv[iarg]);
                usage(argv[0]);
            }
        } else {
            fprintf(stderr, "Error: Unknown argument: %s\n", argv[iarg]);
            usage(argv[0]);
        }
    }

#if defined DEBUG
    printf("Calculate primes up to %d.\n", maxNumber);
    printf("Timer resolution: %d ticks per second.\n", (int) CLOCKS_PER_SEC);
    printf("Word size: %d bits.\n", BITS_PER_WORD);
    printf("\n");
#endif /* defined DEBUG */

    timedTest(secs, primes, "merriam");

    return (0);
}