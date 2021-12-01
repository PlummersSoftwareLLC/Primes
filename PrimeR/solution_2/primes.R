# Prime sieve based on Dave Plummer's algorithm, converted to a functional R style
# Some of this is similar to the original C++ implementation, other functions
# make use of R's vectorisation capability for speed.
# Author: Nick O'Brien


# Store known primes in a dataframe for comparison

knownPrimes <- data.frame(t(matrix(c(10, 4,
                                 100, 25,
                                 1000, 168,
                                 10000, 1229,
                                 100000, 9592,
                                 1000000, 78498,
                                 10000000, 664579,
                                 100000000, 5761455,
                                 1000000000, 50847534,
                                 10000000000, 455052511), nrow = 2)))
# Name the data frame 
names(knownPrimes) <- c("size", "nPrimes")



validateResults <- function() {
  result <- knownPrimes$nPrimes[knownPrimes$size == sieveSize] 
  if (knownPrimes$nPrimes[nrow(knownPrimes)] == result)
    return(FALSE);
  return(result == countPrimes())
}

runSieve <- function() {
  # Maximum number of factors
  maxfactor <- floor(sqrt(sieveSize))
  # Maximum index to look for
  maxfactor_index <- floor(maxfactor*0.5)

  if (maxfactor %% 2 == 0)
    maxfactor_index <- maxfactor_index - 1

  # Walk through each factor, cross off multiples, and increment
  # Here we treat 'factors' as indices in the bit array, which already excludes even numbers 
  # This means we have to transform factors to the actual primes to evaluate them, but this is
  # well worth it for the reduction in comparisons - courtesy of @fvbakel's solution
  # I've changed the while loop to a for loop to reduce conditional checking, which somewhat
  # improves performance

  for (fac in seq.int(from=1, to=maxfactor_index)) {
    if (Bits[fac] == TRUE) {
      # Set factors to false
      prime <- fac * 2 + 1
      start <- ( ((prime * prime )-1)*0.5 )
    # Vectorised subset because R for loops are slow
      Bits[seq.int(from=start, to=bitSize, by=prime)] <<- FALSE
    }
  }
}


printResults <- function(showResults, duration, passes) {
  count <- sum(Bits) + 1
  if (showResults) {
    nOutput <- c(2L, which(Bits) * 2 + 1)
  }
  

  cat(sprintf("Passes: %i, Time: %f, Avg: %f, Limit: %i, Count: %s, Valid: %s\n",
          passes,
          duration,
          duration/passes,
          sieveSize,
          count,
          validateResults()), file = stdout())
  
  cat(sprintf("nobrien97;%d;%f;1;algorithm=base,faithful=no,bits=32\n", passes, duration), file = stdout())
  
  if (showResults) {
    cat(nOutput, "\n", file = stderr())
  }
}

countPrimes <- function(showResults) {
  return(sum(Bits) + 1)
}


# Run the program

passes <- 0
tStart <- proc.time()[3]

while(T) {
  sieveSize <- 1000000
  # Since we don't need to worry about even numbers we can remove them from our bit array
  bitSize <- floor(sieveSize*0.5)
  if (bitSize %% 2 == 0) {
    bitSize <- bitSize - 1
  }
  Bits <- rep(T, bitSize)
  
  runSieve()
  passes <- passes + 1
  tPass <- proc.time()[3]
  if((tPass - tStart) >= 5) {
     printResults(FALSE, (tPass - tStart), passes)
    break
  }
}
