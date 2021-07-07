
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

# Set the counts array
{ counts[$1] = $2; }

function validate_results()
{
    return counts[size] == count_primes();
}

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
            if (show_results) printf("%i, ", i);
            count++;
        }
    }

    if (count != count_primes())
    {
        print "Wrong count of primes";
        exit;
    }

    print "Passes: " passes ", Time: " duration ", Average: " duration / passes ", Count: " count ", Valid: " validate_results();

    print "DaviNakamuraCardoso;" passes ";" duration ";algorithm=base,faithful=yes";
    return;
}

END {
    start = systime();
    passes = 0;

    do {
        reset();
        run_sieve();
        passes++;
        now = systime();
    } while ((now - start) < 5);

    print_results(0, passes, now-start);

}
