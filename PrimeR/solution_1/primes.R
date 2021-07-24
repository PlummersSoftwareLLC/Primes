time_limit <- 5
limit <- 1000000
show_results <- FALSE

knownPrimeCounts <- c(
    "10" = 4,
    "100" = 25,
    "1000" = 168,
    "10000" = 1229,
    "100000" = 9592,
    "1000000" = 78498,
    "10000000" = 664579,
    "100000000" = 5761455
)

prime_sieve <- setRefClass("prime_sieve",
    fields = list (
        # fields declaration as ANY avoids checks on each assignment of new values
        # this matters alot for the performance of the bit_array
        limit_input="ANY",
        limit="ANY",
        bit_size="ANY",
        bit_array="ANY"
    ),
    methods = list (
        # constructor workaround
        init_fields = function(limit_input) {
            limit <<- limit_input
            # some extra logic to handle odd and even limits
            bit_size <<- floor(limit_input * 0.5 ) 
            if (limit_input %% 2 == 0) {
                bit_size <<- bit_size - 1
            }
            bit_array <<- rep(TRUE, bit_size)
        },
        run_sieve = function() {
            # maximum primenumber we need to propagate the prime number
            maxroot <- floor(sqrt(limit))
            # given the max root, what is the maxium index of the array
            # we need to propagate
            maxroot_index <-floor(maxroot * 0.5)
            if (maxroot_index %% 2 == 0) {
                # eg if limit=100, maxroot=10, even numbers have no index
                # only the square root of one odd number below that have 
                # to be propagated
                maxroot_index <- maxroot_index - 1
            } 

            for (factor in seq.int(from=1, to=maxroot_index)) {
                if (bit_array[factor] == TRUE) {
                    # cross out all factors
                    prime <- factor * 2 + 1 
                    start <- ( ((prime * prime) -1) * 0.5 )
                    
                    # propagate
                    bit_array[seq.int(from=start, to=bit_size, by=prime)] <<- FALSE
                } 
            }
        },
        # hard coded add 2
        # convert bit index mumbers back to prime numbers
        bit_array_to_primes = function() {
            return (append(2,(which(bit_array) * 2 + 1)))
        },
        validate_results = function(count) {
            expected <- knownPrimeCounts[sprintf("%i",limit)]
             if (is.na(expected)) {
                return("unkown")
            } else if (count == expected) {
                return("true")
            } else {
                return("false")
            }
        },
        print_results = function(limit, show_results, duration, passes) {
            avg <- duration / passes
            count <- sum(bit_array) +1
            valid <- validate_results(count)

            if (show_results) {
                cat(bit_array_to_primes(),"\n")
            }
            
            cat(sprintf("Passes: %i, Time: %f, Avg: %f (sec/pass), Limit: %i, Count: %i, Valid: %s\n",
                    passes,
                    duration,
                    avg,
                    limit,
                    count,
                    valid
                )
            )
            cat("\n")
            cat(sprintf("fvbakel_R;%i;%f;1;algorithm=base,faithful=yes,bits=32\n",
                    passes,
                    duration
                )
            )
        }
    )
)

main <-function(time_limit,limit,show_results) {
    # get time in sec
    start <- proc.time()[3]
    passes <- 0
    while (TRUE) {
        passes <- passes + 1
        sieve <- prime_sieve$new()
        sieve$init_fields(limit)
        sieve$run_sieve()
        now <- proc.time()[3]
        duration <- now - start
        if (duration > time_limit) {
            sieve$print_results(limit, show_results, duration, passes)
            break
        }
    }
}

main(time_limit,limit,show_results)
