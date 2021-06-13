       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMES.
       AUTHOR. FRANSTEP-SIZE VAN BAKEL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  MAX_LIMIT                      PIC 9(7) VALUE 100.
       77  MAX_ROOT                       PIC 9(7) COMP.
       77  PRIME                          PIC 9(7) COMP.
       77  PRIME-COUNT                    PIC 9(7) COMP.
       77  FACTOR                         PIC 9(7) COMP.
       77  STEP-SIZE                      PIC 9(7) COMP.
       77  START-AT                       PIC 9(7) COMP.
       77  K                              PIC 9(7) COMP.
       77  I                              PIC 9(7) COMP.
       77  BIT_SIZE                       PIC 9(7) COMP.
       01  BIT-ARRAY.
            03 FLAG OCCURS 50 TIMES       PIC 9 COMP.
       PROCEDURE DIVISION.
      * 
       START-UP.
            DISPLAY "Start calculation".
            COMPUTE MAX_ROOT = (MAX_LIMIT ** .5).
            DIVIDE  2 INTO MAX_LIMIT GIVING BIT_SIZE.
            DISPLAY "MAX_ROOT=", MAX_ROOT.
            DISPLAY "MAX_LIMIT=", MAX_LIMIT.
            DISPLAY "BIT_SIZE=", BIT_SIZE.
            PERFORM RUN_SIEVE THROUGH RUN_SIEVE-END.
            DISPLAY "Count starts".
            MOVE 1 TO PRIME-COUNT.
            MOVE 1 TO I.
            PERFORM COUNT-PRIMES UNTIL I = BIT_SIZE.
            DISPLAY "PRIMES FOUND: ", PRIME-COUNT.
            STOP RUN.
      *
       RUN_SIEVE.
            MOVE 1 TO I.
            PERFORM INIT-BITS 100 TIMES.
            MOVE 1 TO FACTOR.
            PERFORM SCAN-FOR-PRIMES UNTIL FACTOR > MAX_ROOT.
            DISPLAY "RUN_SIEVE ready".
       RUN_SIEVE-END.
            EXIT.
      * 
       COUNT-PRIMES.
            IF FLAG (I) = 1
                 THEN
                      ADD 1 TO PRIME-COUNT
                      ADD I I 1 GIVING PRIME
                      DISPLAY "PRIME counted=", PRIME.
            ADD 1 TO I.
       COUNT-PRIMES-END.
            EXIT.
      * 
       INIT-BITS.
            MOVE 1 TO FLAG (I).
            ADD 1 TO I.
       END-INIT-BITS.
            EXIT.
      * 
       SCAN-FOR-PRIMES.
            IF FLAG (FACTOR) = 0
                 THEN
                      GO TO NOT-PRIME.
            ADD FACTOR FACTOR 1 GIVING PRIME.
            ADD FACTOR FACTOR 1 GIVING STEP-SIZE.
      *      COMPUTE START-AT = ( ( ( PRIME * PRIME ) -1 ) * 0.5 ).
            COMPUTE START-AT = (((PRIME * PRIME) - 1) * .5).
            MOVE START-AT TO K.
            DISPLAY "PRIME Calculated=", PRIME.
            DISPLAY "START-AT=", START-AT.
            DISPLAY "STEP-SIZE=", STEP-SIZE.
            DISPLAY "K=", K.
            PERFORM STRIKOUT UNTIL K > BIT_SIZE.
            ADD 1 TO FACTOR.
            DISPLAY "FACTOR=", FACTOR.

      * 
       NOT-PRIME.
            DISPLAY "NOT-PRIME FACTOR=",FACTOR.  
            ADD 1 TO FACTOR.
            DISPLAY "NOT-PRIME FACTOR now=",FACTOR.
            GO TO SCAN-FOR-PRIMES.
      * 
       STRIKOUT.
            DISPLAY "STRIKING OUT K=",K.
            MOVE 0 TO FLAG (K).
            ADD STEP-SIZE TO K.
            DISPLAY "NEXT K=", K.
      * 
       END-PROGRAM.
            EXIT.
