#pragma once

#define REPLICATOR_TYPE typename R_PRIME, typename R_MASK, typename R_SHIFT, typename R_INPUT, int R_PADDING, int R_ALIGN
#define REPLICATOR_ARG R_PRIME, R_MASK, R_SHIFT, R_INPUT, R_PADDING, R_ALIGN

template<REPLICATOR_TYPE>
class Replicator
{
public:
   Replicator(R_PRIME bit);
   Replicator(R_PRIME bit, uint64_t seekIndex, R_INPUT *maskOr);
   ~Replicator() {};
   void ReplicatorInit(R_PRIME bit);
   void ReplicatorInit(R_PRIME bit, uint64_t seekIndex, R_INPUT *maskOr);
   void SeekOr(uint64_t seekIndex, R_INPUT *maskOr);
   void IncrementOr(R_INPUT *maskOr);
#ifdef REPLICATOR_VECTOR
   union {
      R_MASK shift;
      R_SHIFT shiftV;
   };
   R_MASK mask;
#else
   R_MASK mask;
   R_SHIFT shift;
#endif
   R_PRIME prime;
   union {
      R_PRIME decay;
      uint8_t paddy[sizeof(R_PRIME) + R_PADDING];
   };
};

template<REPLICATOR_TYPE>
Replicator<REPLICATOR_ARG>::Replicator(R_PRIME bit)
{
   ReplicatorInit(bit);
};


template<REPLICATOR_TYPE>
Replicator<REPLICATOR_ARG>::Replicator(R_PRIME bit, uint64_t seekIndex, R_INPUT *maskOr)
{
   ReplicatorInit(bit);
   SeekOr(seekIndex, maskOr);
};

template<REPLICATOR_TYPE>
void Replicator<REPLICATOR_ARG>::ReplicatorInit(R_PRIME bit)
{
   prime = (R_PRIME)MAPOUT(bit);
   shift = bit;
   R_PRIME bitIndex = prime;
   mask = 1;
   do {
      mask |= (R_MASK)1 << bitIndex;
      bitIndex += prime;
   } while (bitIndex < (sizeof(R_MASK) << 3));
   decay = prime - bitIndex + (sizeof(R_MASK) << 3);
}

template<REPLICATOR_TYPE>
void Replicator<REPLICATOR_ARG>::ReplicatorInit(R_PRIME bit, uint64_t seekIndex, R_INPUT *maskOr)
{
   ReplicatorInit(bit);
   SeekOr(seekIndex, maskOr);
}

template<REPLICATOR_TYPE>
void Replicator<REPLICATOR_ARG>::SeekOr(uint64_t seekIndex, R_INPUT *maskOr)
{
   int64_t bit = prime >> 1;
   int64_t shiftTemp = ((uint64_t)((prime * prime) >> 1)) - (seekIndex * (sizeof(R_MASK) << 3));
   bit -= decay * seekIndex;
   if (bit < 0)
      bit = bit % prime + prime;
   shift = (R_SHIFT)(bit);
   if (shiftTemp < 0)
      *maskOr |= mask << shift;
   else
      *maskOr |= mask << shiftTemp;
}

template<REPLICATOR_TYPE>
void Replicator<REPLICATOR_ARG>::IncrementOr(R_INPUT *maskOr)
{
   shift -= decay;
   if (true) {
#ifdef R_CAST_SHIFT
      R_SHIFT shift64 = (R_SHIFT)shift;
      shift64 >>= (sizeof(R_SHIFT) << 3) - 1;
      shift64 &= (R_SHIFT)prime;
      shift += shift64;
#else
      shift += (shift >> ((sizeof(R_SHIFT) << 3) - 1)) & prime;
#endif
   } else {
      if (shift < 0)
         shift += prime;
   }
   *maskOr |= mask << shift;
}

//#define REPLICATOR_ARG R_PRIME, R_MASK, R_SHIFT, R_INPUT, R_PADDING, R_ALIGN
#define REPLICATOR_64 int16_t, uint64_t, int32_t, uint64_t, 0, 3
typedef class Replicator<REPLICATOR_64> Replicator64;
