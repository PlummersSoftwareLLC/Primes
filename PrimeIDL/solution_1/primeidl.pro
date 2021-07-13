PRO PRIMESIEVE::RUN

  COMPILE_OPT IDL2

  factor = 3LL
  q = SQRT(self.sieve_size)

  WHILE factor LE q DO BEGIN
    FOR i = factor, self.sieve_size, 2 DO BEGIN
      IF (*self.is_prime)[i/2LL] THEN BEGIN
        factor = i
        BREAK
      ENDIF
    ENDFOR

    FOR i = factor * 3LL, self.sieve_size, factor * 2LL DO $ 
      (*self.is_prime)[i/2LL] = 0B

    factor += 2LL
  ENDWHILE
END

FUNCTION PRIMESIEVE::COUNT_PRIMES

    COMPILE_OPT IDL2

    RETURN,LONG64( $
             TOTAL(*self.is_prime))
END

PRO PRIMESIEVE::LIST_PRIMES

    COMPILE_OPT IDL2

    FOR i = 3LL, self.sieve_size, 2 DO $
      IF (*self.is_prime)[i/2LL] THEN PRINT,FORMAT='(I0,",",$)',i
    PRINT,FORMAT='(A1," ")',STRING(8B)
END

FUNCTION PRIMESIEVE::VALIDATE

    COMPILE_OPT IDL2

    RETURN,self->count_primes() EQ self.prime_counts[self.sieve_size]
END

FUNCTION PRIMESIEVE::INIT,n

  COMPILE_OPT IDL2

  self.prime_counts = HASH(          10LL, 4LL, $
                                    100LL, 25LL, $
                                   1000LL, 168LL, $ 
                                  10000LL, 1229LL, $
                                 100000LL, 9592LL, $
                                1000000LL, 78498LL, $
                               10000000LL, 664579LL, $
                              100000000LL, 5761455LL, $
                             1000000000LL, 50847534LL, $
                            10000000000LL, 455052511LL, /NO_COPY )
  self.sieve_size = n;
  self.is_prime = PTR_NEW( $
                    MAKE_ARRAY((n+1LL)/2LL, /BYTE, VALUE=1B))

  RETURN,1
END

PRO PRIMESIEVE::CLEANUP

    COMPILE_OPT IDL2

    IF PTR_VALID(self.is_prime) THEN $
      PTR_FREE,self.is_prime
END

PRO PRIMESIEVE__DEFINE

  COMPILE_OPT IDL2

  void = {PRIMESIEVE, prime_counts:HASH(), sieve_size:0LL, is_prime:PTR_NEW()}
END

PRO PRIMEIDL

  COMPILE_OPT IDL2

  CPU,TPOOL_NTHREADS=1B,VECTOR_ENABLE=0B

  passes = 0LL
  time_start = SYSTIME(/SECONDS)

  WHILE 1 DO BEGIN
    sieve = OBJ_NEW('PRIMESIEVE', 1000000LL)
    sieve->RUN
    passes++
    time_end = SYSTIME(/SECONDS)
    IF time_end - time_start GE 5D THEN BEGIN
        PRINT,FORMAT='(A0,";",I0,";",F0,";",I0,";algorithm=",A0,";faithful=",A0,";bits=",A0)', $
          'kriztioan', $
          passes, $
          time_end - time_start, $
          1, $
          'base', $
          'yes', $
          'unknown'
        BREAK
    ENDIF
  ENDWHILE
END
