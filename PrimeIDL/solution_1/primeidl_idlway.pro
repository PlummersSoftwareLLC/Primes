PRO PRIMESIEVE::RUN

  COMPILE_OPT IDL2

  factor = 3LL

  WHILE 1 DO BEGIN
    FOR i = factor, self.sieve_size, 2 DO BEGIN
      IF (*self.is_prime)[i/2LL] THEN BEGIN
        factor = i
        BREAK
      ENDIF
    ENDFOR

    factor_squared = factor * factor

    n = self.sieve_size - factor_squared

    IF n LT 0 THEN BREAK

    i = MAKE_ARRAY(1LL + $
                    n / (factor * 2LL), $
                   /INDEX, START=factor_squared, INCREMENT=DOUBLE(factor)*2D, /L64)

    (*self.is_prime)[i/2LL] = 0B

    factor += 2LL
  ENDWHILE
END

FUNCTION PRIMESIEVE::COUNT_PRIMES

    COMPILE_OPT IDL2

    RETURN,TOTAL(*self.is_prime, /INTEGER)
END

PRO PRIMESIEVE::LIST_PRIMES

    COMPILE_OPT IDL2

    FOR i = 3LL, self.sieve_size, 2LL DO $
      IF (*self.is_prime)[i/2LL] THEN PRINT,FORMAT='(I0,",",$)',i
    PRINT,FORMAT='(A1," ")',STRING(8B)
END

FUNCTION PRIMESIEVE::VALIDATE

    COMPILE_OPT IDL2

    ;; work-around for GDL quirk
    DEFSYSV,'!GDL',EXISTS=is_gdl

    counts = is_gdl ? (self.prime_counts[0])[self.sieve_size] $
                    : self.prime_counts[self.sieve_size]

    RETURN,self->count_primes() EQ counts
END

FUNCTION PRIMESIEVE::INIT,n

  COMPILE_OPT IDL2, HIDDEN

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

  self.sieve_size = n
  self.is_prime = PTR_NEW( $
                    MAKE_ARRAY((n+1LL)/2LL, /BYTE, VALUE=1B))

  RETURN,1
END

PRO PRIMESIEVE::CLEANUP

    COMPILE_OPT IDL2, HIDDEN

    IF PTR_VALID(self.is_prime) THEN $
      PTR_FREE,self.is_prime
END

PRO PRIMESIEVE__DEFINE

  COMPILE_OPT IDL2, HIDDEN

  void = {PRIMESIEVE, prime_counts:HASH(), sieve_size:0LL, is_prime:PTR_NEW()}
END

PRO PRIMEIDL_IDLWAY

  COMPILE_OPT IDL2

  passes = 0LL
  time_start = SYSTIME(/SECONDS)

  WHILE 1 DO BEGIN
    sieve = OBJ_NEW('PRIMESIEVE', 1000000LL)
    sieve->RUN
    time_end = SYSTIME(/SECONDS)
    ++passes
    IF time_end - time_start GE 5D THEN BEGIN
        PRINT,FORMAT='(A0,";",I0,";",F0,";",I0,";algorithm=",A0,",faithful=",A0,",bits=",I1)', $
          'kriztioan_idlway', $
          passes, $
          time_end - time_start, $
          !CPU.TPOOL_NTHREADS, $
          'base', $
          'yes', $
          8
        BREAK
    ENDIF
  ENDWHILE
END
