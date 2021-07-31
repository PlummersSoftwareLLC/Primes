PRO PRIMESIEVE::RUN

  COMPILE_OPT IDL2

  factor = 3LL
  q = SQRT(self.sieve_size)

  WHILE factor LE q DO BEGIN
    FOR i = factor, self.sieve_size, 2LL DO BEGIN
      IF self->GET_BIT(i) THEN BEGIN
        factor = i
        BREAK
      ENDIF
    ENDFOR

    FOR i = factor * factor, self.sieve_size, factor * 2LL DO $ 
      self->CLEAR_BIT,i

    factor += 2LL
  ENDWHILE
END

FUNCTION PRIMESIEVE::GET_BIT,i

  COMPILE_OPT IDL2, HIDDEN

  RETURN, ((*self.raw_bits)[i/16LL] AND $
            ISHFT(1B, ((i/2LL - 1LL) MOD 8LL))) GT 0B
END

PRO PRIMESIEVE::CLEAR_BIT,i

  COMPILE_OPT IDL2, HIDDEN

  (*self.raw_bits)[i/16LL] = (*self.raw_bits)[i/16LL] AND $
                                NOT ISHFT(1B, ((i/2LL - 1LL ) MOD 8LL))
END

FUNCTION PRIMESIEVE::COUNT_PRIMES

    COMPILE_OPT IDL2

    ;; work-around for GDL not having BIT_POPULATION
    DEFSYSV,'!GDL',EXISTS=is_gdl

    IF NOT is_gdl THEN $
      RETURN,TOTAL( $
               BIT_POPULATION(*self.raw_bits), /INTEGER) - $
               8LL * N_ELEMENTS(*self.raw_bits) + self.sieve_size / 2LL

    RETURN,TOTAL( $
             BIT_POPULATION(*self.raw_bits), /INTEGER) - $
             8LL * N_ELEMENTS(*self.raw_bits) + self.sieve_size / 2LL
END

PRO PRIMESIEVE::LIST_PRIMES

    COMPILE_OPT IDL2

    FOR i = 3LL, self.sieve_size, 2LL DO $
      IF self->GET_BIT(i) THEN PRINT,FORMAT='(I0,",",$)',i
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
  self.raw_bits = PTR_NEW( $
                    MAKE_ARRAY(((n+1LL)/2LL+7LL)/8LL, /BYTE, VALUE=255B))

  RETURN,1
END

PRO PRIMESIEVE::CLEANUP

    COMPILE_OPT IDL2, HIDDEN

    IF PTR_VALID(self.raw_bits) THEN $
      PTR_FREE,self.raw_bits
END

PRO PRIMESIEVE__DEFINE

  COMPILE_OPT IDL2, HIDDEN

  void = {PRIMESIEVE, prime_counts:HASH(), sieve_size:0LL, raw_bits:PTR_NEW()}
END

PRO PRIMEIDL_1BIT

  COMPILE_OPT IDL2, HIDDEN

  CPU,TPOOL_NTHREADS=1B

  passes = 0LL
  time_start = SYSTIME(/SECONDS)

  WHILE 1 DO BEGIN
    sieve = OBJ_NEW('PRIMESIEVE', 1000000LL)
    sieve->RUN
    time_end = SYSTIME(/SECONDS)
    ++passes
    IF time_end - time_start GE 5D THEN BEGIN
        PRINT,FORMAT='(A0,";",I0,";",F0,";",I0,";algorithm=",A0,",faithful=",A0,",bits=",I1)', $
          'kriztioan_1bit', $
          passes, $
          time_end - time_start, $
          !CPU.TPOOL_NTHREADS, $
          'base', $
          'yes', $
          1
        BREAK
    ENDIF
  ENDWHILE
END
