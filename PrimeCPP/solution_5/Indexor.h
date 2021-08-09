#pragma once

#define INDEXOR_TYPE typename I_BIT, typename I_PRIME, int I_PADDING, int I_ALIGN
#define INDEXOR_ARG I_BIT, I_PRIME, I_PADDING, I_ALIGN

template<INDEXOR_TYPE>
class Indexor
{
public:
   Indexor(I_PRIME sprime = 0)
   {
      IndexorInit(sprime);
   }
   ~Indexor() {};
   void IndexorInit(I_PRIME sprime = 0)
   {
      dprime = MAPOUT(sprime);
      //bit = bitbase;
      bit = dprime * dprime;
      dprime <<= 1;
   }
   void Update(uint8_t *base)
   {
      I_BIT halfBit = bit >> 1;
      WRITEBIT(base, halfBit);
      bit += dprime;

   }
   bool Update(uint8_t *base, I_BIT highBit)
   {
      I_BIT halfBit = bit >> 1;
      if (halfBit < highBit) {
         WRITEBIT(base, halfBit);
         bit += dprime;
         return true;
      }
      return false;
   }
   void UpdateTo(uint8_t *base, I_BIT highBit)
   {
      I_BIT halfBit = bit >> 1;
      while (halfBit < highBit) {
         WRITEBIT(base, halfBit);
         bit += dprime;
         halfBit = bit >> 1;
      }
   }
   I_BIT bit;
   union {
      I_PRIME dprime;
      uint8_t paddy[sizeof(I_PRIME) + I_PADDING];
   };
};

#define INDEXOR_64 uint64_t, uint64_t, 0, 6
typedef class Indexor<INDEXOR_64> Indexor64;
