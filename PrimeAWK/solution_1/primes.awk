
# This runs before scanning the input file (counts.csv)
BEGIN {

    # There are no classes or structs in Awk, so variables are global
    size = 1000000;
    for (i = 0; i < size; i++)
    {
        rawbits[i] = 1;
    }

}

function reset()
{
    # Reset the bit array
    for (i = 0; i < size; i+=2)
    {
        rawbits[i] = 1;
    }
}

# AWK needs an input file to operate on, so we use "counts.csv" to set hashmap of known prime counts
# This is setting the value of the first column ($1) to the second column ($2)
{ counts[$1] = $2; }

function validate_results()
{
    return counts[size] == count_primes();
}

# Same count method as Dave's CPP implementation
function count_primes()
{
    counter = 1;
    for (i = 3; i < size; i+=2)
    {
        if (rawbits[i] == 1) counter++;
    }

    return counter;
}

# Same implementation showed by Dave in his CPP algorithm
function run_sieve()
{
    factor = 3;
    q = int(sqrt(size));

    while (factor <= q)
    {
        for (i = factor; i < size; i+=2)
        {
            if (rawbits[i] == 1)
            {
                factor = i;
                break;
            }
        }

        for (i = factor*3; i < size; i += factor*2)
        {
            rawbits[i] = 0;
        }

        factor = factor + 2;
    }


    return;
}

function print_results(show_results, passes, duration)
{
    if (show_results)
        printf("2, ");

    count = 1;
    for (i = 3; i < size; i+=2)
    {
        if (rawbits[i] == 1)
        {
            # AWK has support for some functions from the C library, such as printf
            if (show_results) printf("%i, ", i);
            count++;
        }
    }

    if (count != count_primes())
    {
        # Assert the count
        print "Wrong count of primes";
        exit;
    }

    print "Passes: " passes ", Time: " duration ", Average: " duration / passes ", Count: " count ", Valid: " validate_results();

    print "DaviNakamuraCardoso;" passes ";" duration ";1;algorithm=base,faithful=no";
    return;
}

END {
    start = systime();
    passes = 0;

    do {
        # Reset the bitarray each iteration
        reset();

        # Run
        run_sieve();
        passes++;
        
        now = systime();
    } while ((now - start) < 5);

    print_results(0, passes, now-start);

}
