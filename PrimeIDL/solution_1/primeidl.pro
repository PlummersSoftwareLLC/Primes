PRO PRIMESIEVE::RUN

  factor = 3LL
  q = SQRT(self.sieve_size)

  WHILE factor LE q DO BEGIN
    FOR i = factor, self.sieve_size, 2 DO BEGIN
      IF self->GET_BIT(i) THEN BEGIN
        factor = i
        BREAK
      ENDIF
    ENDFOR

    FOR i = factor * 3LL, self.sieve_size, factor * 2LL DO $ 
      self->CLEAR_BIT,i

    factor += 2
  ENDWHILE
END

FUNCTION PRIMESIEVE::GET_BIT,idx
    RETURN,(*self.raw_bits)[idx/2LL]
END

PRO PRIMESIEVE::CLEAR_BIT,idx
    (*self.raw_bits)[idx/2LL] = 0B
END

FUNCTION PRIMESIEVE::COUNT_PRIMES
    RETURN,LONG64( $
             TOTAL(*self.raw_bits))
END

FUNCTION PRIMESIEVE::VALIDATE
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
  self.raw_bits = PTR_NEW( $
                    MAKE_ARRAY((n+1LL)/2LL, /BYTE, VALUE=1B))

  RETURN,1
END

PRO PRIMESIEVE::CLEANUP
    IF PTR_VALID(self.raw_bits) THEN $
      PTR_FREE,self.raw_bits
END

PRO PRIMESIEVE__DEFINE

  COMPILE_OPT IDL2

  void = {PRIMESIEVE, prime_counts:HASH(), sieve_size:0LL, raw_bits:PTR_NEW()}
END

PRO PRIMEIDL

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
