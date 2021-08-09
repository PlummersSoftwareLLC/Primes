#pragma once

//#define PRIMEBUCKETS
#ifdef  PRIMEBUCKETS
#define PRIMEBUCKETS_TYPE typename P_WORD, typename P_BUCKET
#define PRIMEBUCKETS_ARG P_WORD, P_BUCKET

template<PRIMEBUCKETS_TYPE>
class PrimeBuckets
{
public:
   PrimeBuckets() : count(0) {};
   void AddBucket(P_BUCKET bits, P_BUCKET offset)
   {
      buckets.emplace_back();
      vector<P_BUCKET> *bucket = &buckets.back();
      P_WORD bitIndex = 0;
      do {
         if ((~bits) & ONESHIFT(bitIndex)) {
            bucket->emplace_back((P_BUCKET)MAPOUT(bitIndex + offset));
            count++;
         }
      } while (++bitIndex < (sizeof(P_WORD) << 3));
   };
   P_WORD count;
   vector<vector<P_BUCKET>> buckets;
};

#define PRIMEBUCKETS_64 uint64_t, uint64_t
typedef class PrimeBuckets<PRIMEBUCKETS_64> PrimeBuckets64;

#endif

