#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <stdbool.h>
#include <time.h>

#define Sieve_size 10000000

int count_primes(const bool *bits) {
    int count = 1;
    for(int i = 3; i < Sieve_size; i += 2)
        if (bits[i] == true)
            count++;
    return count;
}

void sieve(){
    bool *bits = malloc(Sieve_size * sizeof(bool));
    for(int i = 0; i < Sieve_size; ++i) {
        bits[i] = true;
    }
    int factor = 3;
    int q = sqrt(Sieve_size);
    while (factor <= q) {
        for(int num = factor; num < Sieve_size; num += 2) {
            if(bits[num] == true){
                factor = num;
                break;
            }
        }
        for(int num = factor * factor; num < Sieve_size; num += factor * 2)
            bits[num] = false;
        factor += 2;
    }
    free(bits);
    bits = NULL;
}

int main() {
    int passes = 0;
    FILE *fp = fopen("primes.csv", "a");
    clock_t start = clock();
    while (true) {
        sieve();
        passes++;
        if((clock() - start) / CLOCKS_PER_SEC >= 5) {
            fprintf(fp, "C, %d\n", passes);
            fclose(fp);
            break;
        }
    }
    printf("passes: %d\n", passes);
    return 0;
}