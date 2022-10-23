static inline void printWord(bitword_t bitword)
{
    char row[WORD_SIZE*2] = {};
    int col=0;
    for (int i=WORD_SIZE-1; i>=0; i--) {
		row[col++] = (bitword & (SAFE_SHIFTBIT<<i))?'1':'.';
		if (!(i%8)) row[col++] = ' ';
    }

    printf("%s", row);
}

static inline void printBits(size_t const size, void const * const ptr)
{
    unsigned char *b = (unsigned char*) ptr;
    unsigned char byte;
    int i, j;

    for (i = size-1; i >= 0; i--) {
        for (j = 7; j >= 0; j--) {
            byte = (b[i] >> j) & 1;
            if (byte==0) printf(".");
            else printf("%u", byte);
        }
        printf(" ");
    }
}

static inline void dump_bitarray(struct sieve_state *sieve) {
    counter_t       bits = sieve->bits;
    bitword_t  *bitarray = sieve->bitarray;
    printf("Dumping sieve with %ld bits\n",(uint64_t)bits);
    for (counter_t word=0; word <= 1+(bits >> SHIFT); word+=2) {
        printf("word %2ld %2ld ",(uint64_t)word+1,(uint64_t)word);
        printBits(8,&bitarray[word]);
        printf(" %6ld - %6ld\n", (uint64_t)word*WORD_SIZE+WORD_SIZE+WORD_SIZE-1,(uint64_t) word*WORD_SIZE);

    }
    puts("");
}
