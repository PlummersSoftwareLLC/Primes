#pragma once

#define SIEVEOR_TYPE typename S_WORD, class S_REPLICATOR, class S_INDEXOR, int S_ALIGNMENT
#define SIEVEOR_ARG S_WORD, S_REPLICATOR, S_INDEXOR, S_ALIGNMENT

template<SIEVEOR_TYPE>
class Seiveor
{
public:
   Seiveor();
   ~Seiveor();
   int Allocate(uint64_t maxBit);
   int Sievey(uint64_t maxBit, uint64_t subDivisions = 1);
   S_WORD count;
   uint8_t *base;
   S_WORD *indexes;
   S_WORD bits;

   void printResults(uint64_t maxBit, bool showResults, double duration, int passes);
private:
   bool validateResults(uint64_t maxBit);
   int countPrimes(uint64_t maxBit, bool showResults = false);
   static const std::map<const long long, const int> resultsDictionary;
};

template<SIEVEOR_TYPE>
Seiveor<SIEVEOR_ARG>::Seiveor()
   :count(0), base(0)
{
   //Allocate(0);
};

template<SIEVEOR_TYPE>
Seiveor<SIEVEOR_ARG>::~Seiveor()
{
   if (base)
      free(base);
};

template<SIEVEOR_TYPE>
int Seiveor<SIEVEOR_ARG>::Allocate(uint64_t maxBit)
{
   if (count < maxBit) {
      uint64_t maxCount = 1005300;
      count = MAXLAM(maxBit, maxCount);
      if (count & 1)
         count++;
      S_WORD baseSize = 1 << 16;
      if (count != maxCount) {
         // x = n * (1.2 - 0.023 * ((log n) - 3))
         // min allocation 1005300 / 16 = 62831 + sizeof(S_INDEXOR) = 16 * 169 = 65535 + align = 64k
         // at sqrt(10^18) = 10^9 base over allocation is ~0.5% est 51098424 actual 50847534
         double pntEst = MAXLAM(sqrt(count), sqrt(maxCount));
         pntEst *= 1.2 - 0.023 * (log10(pntEst) - 3);
         S_WORD baseSize = (S_WORD)(count >> 4) + sizeof(S_INDEXOR) * (S_WORD)(pntEst / log(pntEst));
         baseSize += TO_ALIGNED(baseSize, ((S_WORD)LOG2(sizeof(S_WORD))));
      }
      if (base) {
         uint8_t *nextBase = (uint8_t *)realloc(base, baseSize);
         if (!nextBase) {
            free(base);
            base = 0;
            count = 0;
            return -1;
         }
         base = nextBase;
      } else {
         base = (uint8_t *)malloc(baseSize);
         if (!base) {
            count = 0;
            return -1;
         }
      }
      indexes = reinterpret_cast<S_WORD *>(base + baseSize) - 1;
      bits = (S_WORD)base << 3;
   }
   return 0;
}

template<SIEVEOR_TYPE>
int Seiveor<SIEVEOR_ARG>::Sievey(uint64_t maxBit, uint64_t subDivisions)
{
   if (maxBit < 128)
      return -1;
   if (Allocate(maxBit))
      return -2;
#ifdef PRIMEBUCKETS
   PrimeBuckets64 primeBuckets;
#endif
   S_WORD logS = LOG2(sizeof(S_WORD));
   S_WORD shift = logS + 1;
   uint64_t adjOff = maxBit >> shift;
   if (maxBit & MASK(shift))
      adjOff++;
   adjOff += TO_ALIGNED(adjOff, S_ALIGNMENT);
   S_WORD *wordStart = reinterpret_cast<S_WORD *>(base);
   S_WORD *word = wordStart;
   S_REPLICATOR *replicators = reinterpret_cast<S_REPLICATOR *>(base + adjOff);
   S_REPLICATOR *replicator = replicators;
   S_WORD stride = ONESHIFT(logS + 3);
   S_WORD bit = 1; // bits + 1;
   S_WORD bitWordEnd = stride;
   S_WORD bitStart = BITSTART(1);
   adjOff >>= logS;
   S_WORD *wordEnd = word + adjOff;
   adjOff = ONESHIFT(logS + 2);
   S_WORD *replicatorEnd = MINLAM(wordEnd, word + adjOff);
   S_WORD bitIndex = *word = 1;
   S_WORD wordIndex = 0;
   do {
      while (bitStart < bitWordEnd) {
         if (!READBIT(base, bit))
#ifdef S_NO_NEW
            replicator++->ReplicatorInit(bitIndex, wordIndex, word);
#else
            new (replicator++) S_REPLICATOR(bitIndex, wordIndex, word);
#endif 
         bitIndex++;
         bitStart = BITSTART(bitIndex);
         bit++;
      }
#ifdef PRIMEBUCKETS
      primeBuckets.AddBucket(*word, wordIndex << 6);
#endif
      if (++word >= replicatorEnd)
         break;
      wordIndex++;
      *word = 0;
      S_REPLICATOR *replicate = replicators;
      do {
         replicate++->IncrementOr(word);
      } while (replicate < replicator);
      bitWordEnd += stride;
   } while (true);
   if (word < wordEnd) {
      S_INDEXOR *indexors = reinterpret_cast<S_INDEXOR *>(replicator);
      S_INDEXOR *indexor = indexors;
      subDivisions = MAX(1, subDivisions);
      S_WORD wordSubDivisions = subDivisions + (wordEnd - word) / subDivisions;
      do {
         S_WORD *wordRun = word + wordSubDivisions;
         if (wordRun > wordEnd)
            wordRun = wordEnd;
         do {
            *word = 0;
            if (true) {
               S_REPLICATOR *replicate = replicators;
               do {
                  replicate++->IncrementOr(word);
               } while (replicate < replicator);
            }
         } while (++word < wordRun);
         S_WORD bitRun = (word - wordStart) << (logS + 3);
         while (bitStart < bitRun) {
            if (!READBIT(base, bit))
#ifdef S_NO_NEW
               indexor++->IndexorInit(bit); // s, bitIndex);
#else
               new (indexor++) S_INDEXOR(bit); // s, bitIndex);
#endif 
            bitIndex++;
            bitStart = BITSTART(bitIndex);
            bit++;
            S_INDEXOR *index = indexors;
            while (index < indexor)
               index++->Update(base);
         }
         // bitRun += bits;
         S_INDEXOR *index = indexors;
         do {
            index++->UpdateTo(base, bitRun);
         } while (index < indexor);
      } while (word < wordEnd);
#ifdef PRIMEBUCKETS
      word = replicatorEnd;
      do {
         primeBuckets.AddBucket(*word, (++wordIndex) << 6);
      } while (++word < wordEnd);
#endif
#ifndef S_NO_NEW
      S_INDEXOR *index = indexors;
      while (index < indexor)
         index++->~S_INDEXOR();
   }
   S_REPLICATOR *replicate = replicators;
   do {
      replicate++->~S_REPLICATOR();
   } while (replicate < replicator);
#else
}
#endif
   return 0;
}

template<SIEVEOR_TYPE>
const std::map<const long long, const int> Seiveor<SIEVEOR_ARG>::resultsDictionary =
{
    {          10LL, 4         },               // Historical data for validating our results - the number of primes
    {         100LL, 25        },               // to be found under some limit, such as 168 primes under 1000
    {        1000LL, 168       },
    {       10000LL, 1229      },
    {      100000LL, 9592      },
    {     1000000LL, 78498     },
    {    10000000LL, 664579    },
    {   100000000LL, 5761455   },
    {  1000000000LL, 50847534  },
    { 10000000000LL, 455052511 },
};

template<SIEVEOR_TYPE>
bool Seiveor<SIEVEOR_ARG>::validateResults(uint64_t maxBit)
{
   auto result = resultsDictionary.find(maxBit);
   if (resultsDictionary.end() == result)
      return false;
   return result->second == countPrimes(maxBit);
}

template<SIEVEOR_TYPE>
void Seiveor<SIEVEOR_ARG>::printResults(uint64_t maxBit, bool showResults, double duration, int passes)
{
   if (showResults)
      printf("2, ");

   int count = countPrimes(maxBit, showResults);
   printf("Passes: %d, Time: %lf, Avg: %lf, Limit: %ld, Count1: %d, Count2: %d, Valid: %d\n",
      passes,
      duration,
      duration / passes,
      maxBit,
      count, count,
      validateResults(maxBit));

   // Following 2 lines added by rbergen to conform to drag race output format
   printf("\n");
   printf("davepl_pol;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", passes, duration);
}

template<SIEVEOR_TYPE>
int Seiveor<SIEVEOR_ARG>::countPrimes(uint64_t maxBit, bool showResults)
{
   maxBit >>= 1;
   S_WORD bit = 1; 
   S_WORD bitEnd = maxBit; 
   S_WORD count = 1;
   do {
      if (!READBIT(base, bit)) {
         if (showResults)
            printf("%d, ", MAPOUT(bit)); 
         count++;
      }
   } while (++bit < bitEnd);
   if (showResults)
      printf("\n");
   return count;
}

#define SIEVEOR_64 uint64_t, Replicator64, Indexor64, 3
typedef class Seiveor<SIEVEOR_64> Seiveor64;

