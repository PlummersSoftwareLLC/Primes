// used only for debugging
static inline void __attribute__ ((cold))
printWord_uint64(uint64_t bitword)
{
  verbose1(
    printf("\n");
    char row[64*2] = {};
    int col=0;
    for (int i=64-1; i>=0; i--) {
      row[col++] = (bitword & (1ULL<<i))?'1':'.';
      if (!(i%8)) row[col++] = ' ';
    }

  printf("%s", row); 
  )
}

static inline void __attribute__ ((cold))
printWord_uint32(uint64_t bitword)
{
  verbose1(
    char row[32*2] = {};
      int col=0;
      for (int i=32-1; i>=0; i--) {
        row[col++] = (bitword & (1ULL<<i))?'1':'.';
        if (!(i%8)) row[col++] = ' ';
      }
    printf("%s", row); 
  )
}

#define PRINT_VECTOR_ELEMENTS 4
#define PRINT_WORD_SIZE_BITS 64

static void __attribute__ ((cold)) 
printVector(uint64v4_t bitvector)
{
  verbose1(
    // Use a union to extract the scalar elements from the vector
    union {
      uint64v4_t vec;
      uint64_t   arr[PRINT_VECTOR_ELEMENTS];
    } u;
    u.vec = bitvector;

    char row[PRINT_VECTOR_ELEMENTS*PRINT_WORD_SIZE_BITS*2] = {0};
    char notes[400] = "\0";
    int col = 0;
    // Each vector element is a bitword_t with WORD_SIZE bits
    for (int j = PRINT_VECTOR_ELEMENTS - 1; j >= 0; j--) {
        for (int i = PRINT_WORD_SIZE_BITS - 1; i >= 0; i--) {
            row[col++] = (u.vec[j] & (1ULL << i)) ? '1' : '.';
            if (i % 8 == 0)
                row[col++] = ' ';
        }
        row[col++] = 'x'; row[col++] = ' ';
      }

    for (int j = PRINT_VECTOR_ELEMENTS - 1; j >= 0; j--) {
      for (int i = PRINT_WORD_SIZE_BITS - 1; i >= 0; i--) {
         if (u.arr[j] & (1ULL << i)) sprintf(notes, "%s %ju", notes, (uintmax_t) i + j*PRINT_WORD_SIZE_BITS  );
      }
    }

    row[col] = '\0';
    printf("%s %s\n", row, notes); 
  )
}

static void __attribute__ ((cold)) printVectorNumeric(uint64v4_t bitvector)
{
  for(counter_t i=0; i < PRINT_VECTOR_ELEMENTS; i++) {
      verbose1( printf("%ju,", (uintmax_t) bitvector[i]); )
  }
  verbose1( printf("\n");	)
}

#undef PRINT_VECTOR_ELEMENTS
#undef PRINT_WORD_SIZE_BITS