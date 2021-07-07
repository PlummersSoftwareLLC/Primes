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
  fac <- 3
  # Maximum number of factors
  q <- floor(sqrt(sieveSize))
  # Walk through each factor, cross off multiples, and increment
  while (fac <= q) {
    # Vectorised assignment because R for loops are slow
    notPrime <- seq.int(fac*fac, sieveSize, fac*2)
    Bits[notPrime] <<- FALSE

    fac <- fac + 2
  }
}


printResults <- function(showResults, duration, passes) {
  count <- (sieveSize >= 2)
  
  if (showResults) {
    nOutput <- 2
  }
  
  for (i in seq(3, sieveSize+1, 2)) {
    if (Bits[i] %in% TRUE) {
      count <- count + 1
      if (showResults)
        nOutput <- c(nOutput, i)
    }
  }
  
  cat(sprintf("Passes: %i, Time: %f, Avg: %f, Limit: %i, Count: %s, Valid: %s\n",
          passes,
          duration,
          duration/passes,
          sieveSize,
          countPrimes(),
          validateResults()), file = stdout())
  
  cat(sprintf("nobrien97;%d;%f;1;algorithm=base,faithful=no,bits=32\n", passes, duration), file = stdout())
  
  if (showResults) {
    cat(nOutput, "\n", file = stderr())
  }
  
  
}

countPrimes <- function(showResults) {
  count <- (sieveSize >= 2)
  isPrimeSeq <- seq(3, sieveSize, 2)
  count <- sum(count, Bits[isPrimeSeq] %in% TRUE)
  return(count)
}


# Run the program

passes <- 0
tStart <- proc.time()


while(T) {
  sieveSize <- 1000000
  Bits <- rep(T, sieveSize)
  
  runSieve()
  passes <- passes + 1
  tPass <- proc.time()
  if((tPass[3] - tStart[3]) >= 5) {
     printResults(FALSE, (tPass[3] - tStart[3]), passes)
    break
  }
}
