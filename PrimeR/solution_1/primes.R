#print("Hello World",quote=FALSE)

prime_sieve <- setRefClass("prime_sieve",
    fields = list (
        limit_input="numeric"
    ),
    methods = list (
        # constructor workaround
        init_fields = function(limit_input) {
            limit <<- limit_input
            # some extra logic to handle odd and even limits
            bit_size <<- floor(limit_input / 2) + (floor(limit_input %% 2) - 1)
            bit_array <<- rep(TRUE, bit_size)
        },
        run_sieve = function(prime_sieve) {
            maxroot <- floor(sqrt(limit))
            maxroot_index <-floor(maxroot /2) 

            cat(sprintf("limit=%i\n,bit_size=%i\n,maxroot=%i\n,maxroot_index=%i\n",limit,bit_size,maxroot,maxroot_index))

            factor <- 1
            while(factor <= maxroot_index){
                # cross out all factors
                prime <- factor * 2 +1
                start <- ( ((prime * prime) -1) /2 )
                step <-  factor * 2 + 1
                cat(sprintf("processing prime=%i,start=%i (=%i), step=%i (=%i)\n",prime,start,(start*2 +1),step,(step*2 +1) ))

                bit_array[seq.int(from=start, to=bit_size, by=step)] <- FALSE
                # determine new factor
                factor <- factor + min(which(bit_array[(factor + 1) : limit]))
            }
            # store the calculated new array
            bit_array <<-bit_array
        },
        # hard coded add 2
        # convert bit index mumbers back to prime numbers
        bit_array_to_primes = function(bit_array_to_primes) {
            return (append(2,(which(bit_array) * 2 + 1)))
        }
    )
)

sieve <- prime_sieve$new()
sieve$init_fields(96)
sieve$run_sieve()
sieve$bit_array_to_primes()

print("Bye Bye",quote=FALSE)