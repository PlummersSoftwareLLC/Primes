#pragma once

// note below must match size
#define BITORDER 6 // ... 5 = 2^5 = 32, 6 = 2^6 = 64 ...
#define WORDTYPE int64_t 
#define UWORDTYPE uint64_t 

#define ONETYPE ( (UWORDTYPE)(1) )
template<typename T>
constexpr auto ONESHIFT(T a) { return (ONETYPE << (UWORDTYPE)(a)); }
#define MASK(a) ( ONESHIFT(a) - ONETYPE )
#define BITS(a) ( (a) & MASK(BITORDER))
#define WORDS(a) ( (a) >> BITORDER)  // byte word dword qword lane etc
#define MAPOUT(a) ( ( (UWORDTYPE)(a) << ONETYPE ) + ONETYPE)
#define MAPIN(a) ( ( (a) - ONETYPE ) >> ONETYPE)
#define MIN(a,b) ( ( (a) < (b) ) ? (a) : (b) )
#define MAX(a,b) ( ( (a) > (b) ) ? (a) : (b) )

#define MINLAM(a, b) ( [=]() { return ( a < b ? a : b );  } () )
#define MAXLAM(a, b) ( [=]() { return ( a > b ? a : b );  } () )

//#define WRITEBIT(a) *reinterpret_cast<uint8_t *>((a) >> 3) |= 1 << ((a) & 7)
//#define READBIT(a) (*reinterpret_cast<uint8_t *>((a) >> 3) & (1 << ((a) & 7)))

template<typename T1, typename T2>
constexpr auto WRITEBIT(T1 a, T2  b) { return ( a[(b) >> 3] |= (1 << ((b) & 7)) ); }
template<typename T1, typename T2>
constexpr auto READBIT(T1 a, T2  b) { return ( a[(b) >> 3] & (1 << ((b) & 7)) ); }

template<typename T>
constexpr auto BITSTART(T a) { auto aPrime = MAPOUT(a); return ((aPrime * aPrime) >> 1); }

#define TO_ALIGNED(a,b)  ( (-(WORDTYPE)(a)) & MASK(b) )
#define LOG2(a) ( log( (a) ) / log(2) )

