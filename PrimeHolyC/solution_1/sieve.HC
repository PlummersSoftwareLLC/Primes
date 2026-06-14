// Holy C Sieve of Eratosthenes - Base Algorithm

U64 USqrt(U64 n)
{
    U64 x = n / 2;
    U64 x_last = 0;

    while (x != x_last) {
        x_last = x;
        x = (x + n / x) / 2;
    }
    return x;
}

U64 RunSieve(U64 max_num, U8 *buffer)
{
    U64 i;
    U64 j;
    U64 sqrt_max;
    U64 count = 0;
    
    // Initialize all as prime
    for (i = 2; i < max_num; i++) {
        buffer[i] = 1;
    }
    buffer[0] = 0;
    buffer[1] = 0;
    
    sqrt_max = USqrt(max_num);
    
    // Sieve of Eratosthenes - basic algorithm
    for (i = 2; i <= sqrt_max; i++) {
        if (buffer[i]) {
            // Mark all multiples of i as not prime
            for (j = i * i; j < max_num; j += i) {
                buffer[j] = 0;
            }
        }
    }
    
    // Count primes
    for (i = 2; i < max_num; i++) {
        if (buffer[i]) {
            count++;
        }
    }
    
    return count;
}

U64 ValidateResult(U64 count, U64 max_num)
{
    // Known: there are 78498 primes less than 1000000
    if (max_num == 1000000) {
        return count == 78498;
    }
    return 1;
}

U64 main(U64 argc, U8 **argv)
{
    U64 max_number = 1000000;
    U8 *buffer;
    U64 prime_count;
    
    buffer = malloc(max_number);
    
    // Run the sieve algorithm once
    prime_count = RunSieve(max_number, buffer);
    
    // Validate result
    if (!ValidateResult(prime_count, max_number)) {
        printf("ERROR: Invalid prime count %lu for max %lu\n", prime_count, max_number);
        free(buffer);
        return 1;
    }
    
    // Just output the count - shell wrapper handles timing and iteration counting
    printf("%lu\n", prime_count);
    
    free(buffer);
    
    return 0;
}
