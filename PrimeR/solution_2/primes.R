# Prime sieve from Dave Plummer's C++ code, converted to a functional R style

sieveSize <- 0

Bits <- T

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

names(knownPrimes) <- c("size", "nPrimes")

validateResults <- function() {
  result <- knownPrimes$nPrimes[knownPrimes$size == sieveSize] 
  if (knownPrimes$nPrimes[nrow(knownPrimes)] == result)
    return(FALSE);
  return(result == countPrimes())
}

runSieve <- function() {
  fac <- 3
  q <- floor(sqrt(sieveSize))
  
  while (fac <= q) {
    for (i in seq(fac, sieveSize, 2)) {
      if (Bits[i] %in% TRUE) {
        fac <- i
        break
      }
    }
    for (i in seq(fac*fac, sieveSize, fac*2)) {
      Bits[i] <<- FALSE
    }
    
    fac <- fac + 2
  }
}

printResults <- function(bShowResults, fDuration, iPasses) {
  if (bShowResults)
    print(sprintf("2, "))
  
  count <- (sieveSize >= 2)
  
  for (i in seq(3, sieveSize+1, 2)) {
    if (Bits[i] %in% TRUE) {
      if (bShowResults)
        print(sprintf("%d, ", i))
      count <- count + 1
    }
  }
  if (bShowResults)
    print(sprintf("\n"))
  
  print(sprintf("Passes: %d, Time: %f, Avg: %f, Limit: %d, Count1: %d, Count2: %d, Valid: %s\n",
          iPasses,
          fDuration,
          fDuration/passes,
          sieveSize,
          count,
          countPrimes(),
          validateResults()))
  
  print(sprintf("\n"))
  print(sprintf("nobrien97;%d;%f;1;algorithm=base,faithful=yes,bits=1\n", iPasses, fDuration))
}

countPrimes <- function() {
  count <- (sieveSize >= 2)
  for (i in seq(3, sieveSize, 2))
    if (Bits[i] %in% TRUE | Bits[i] %in% NA)
      count <- count + 1
  return(count)
}


# Run the program

passes <- 0
tStart <- proc.time()


while(T) {
  sieveSize <- 1000000
  Bits <- T
  
  runSieve()
  passes <- passes + 1
  if((proc.time()[3] - tStart[3]) >= 5) {
     printResults(TRUE, (proc.time()[3] - tStart[3]) / 1000000, passes)
    break
  }
  
}
