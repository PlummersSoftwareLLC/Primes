       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMES.
       AUTHOR. FRANK VAN BAKEL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  MAX_LIMIT                      PIC 9(7) VALUE 1000000.
       77  SHOW_RESULTS                   PIC 9(2) VALUE 0. 
       77  MAX_ROOT                       PIC 9(7) COMP.
       77  MAX_ROOT_INDEX                 PIC 9(7) COMP.
       77  PRIME                          PIC 9(7) COMP.
       77  PRIME-COUNT                    PIC 9(7) COMP.
       77  FACTOR                         PIC 9(7) COMP.
       77  STEP-SIZE                      PIC 9(7) COMP.
       77  START-AT                       PIC 9(7) COMP.
       77  K                              PIC 9(7) COMP.
       77  I                              PIC 9(7) COMP.
       77  IS-EVEN                        PIC 9(1)v9.
       77  BIT_SIZE                       PIC 9(7) COMP.
       01  BIT-ARRAY.
            03 FLAG OCCURS 500000 TIMES     PIC 9(1) COMP.
       PROCEDURE DIVISION.
      * 
       START-UP.
            DISPLAY "Start calculation".
            PERFORM RUN_SIEVE THROUGH END-RUN_SIEVE.
            DISPLAY "Count starts".
            MOVE 1 TO PRIME-COUNT.
            MOVE 1 TO I.
            PERFORM COUNT-PRIMES UNTIL I > BIT_SIZE.
            DISPLAY "PRIMES FOUND: ", PRIME-COUNT.
            STOP RUN.
      *
       RUN_SIEVE.
            COMPUTE MAX_ROOT ROUNDED = (MAX_LIMIT ** .5).
            DIVIDE 2 INTO MAX_ROOT 
               GIVING MAX_ROOT_INDEX ROUNDED
               REMAINDER IS-EVEN.
            IF IS-EVEN = 0 THEN 
                   DISPLAY "MAX_ROOT is even IS-EVEN=,"IS-EVEN
                   ADD -1 TO MAX_ROOT_INDEX.

            DIVIDE 2 INTO MAX_LIMIT
               GIVING BIT_SIZE ROUNDED
               REMAINDER IS-EVEN.
            IF IS-EVEN = 0 THEN 
                   DISPLAY "MAX_LIMIT is even IS-EVEN=,"IS-EVEN
                   ADD -1 TO BIT_SIZE.    
            DISPLAY "MAX_ROOT=", MAX_ROOT.
            DISPLAY "MAX_ROOT_INDEX=",MAX_ROOT_INDEX.
            DISPLAY "MAX_LIMIT=", MAX_LIMIT.
            DISPLAY "BIT_SIZE=", BIT_SIZE.
      *     Set all values in the array to 1      
            MOVE 1 TO I.
            PERFORM INIT-BITS BIT_SIZE TIMES.
            MOVE 1 TO FACTOR.
      *     Outer loop 
            PERFORM SCAN-FOR-PRIMES UNTIL FACTOR > MAX_ROOT_INDEX.
            DISPLAY "RUN_SIEVE ready".
       END-RUN_SIEVE.
            EXIT.
      * 
       COUNT-PRIMES.
            IF FLAG (I) = 1 THEN
               ADD 1 TO PRIME-COUNT
               ADD I I 1 GIVING PRIME
               IF SHOW_RESULTS = 1 THEN
                   DISPLAY "PRIME found=", PRIME.
            ADD 1 TO I.
       END-COUNT-PRIMES.
            EXIT.
      * 
       INIT-BITS.
            MOVE 1 TO FLAG (I).
            ADD 1 TO I.
       END-INIT-BITS.
            EXIT.
      * 
       SCAN-FOR-PRIMES.
            IF FLAG (FACTOR) = 1 THEN
                   ADD FACTOR FACTOR 1 GIVING PRIME
                   ADD FACTOR FACTOR 1 GIVING STEP-SIZE
                   COMPUTE START-AT = (((PRIME * PRIME) - 1) / 2)
                   MOVE START-AT TO K
                   DISPLAY "PRIME Calculated=", PRIME
                   DISPLAY "START-AT=", START-AT
                   DISPLAY "STEP-SIZE=", STEP-SIZE
                   PERFORM STRIKOUT UNTIL K > BIT_SIZE.
            ADD 1 TO FACTOR.
            DISPLAY "FACTOR=", FACTOR.
       END-SCAN-FOR-PRIMES.
           EXIT.
      * 
       STRIKOUT.
      *      DISPLAY "STRIKING OUT K=",K.
            MOVE 0 TO FLAG (K).
            ADD STEP-SIZE TO K.
      *      DISPLAY "NEXT K=", K.
       END-STRIKOUT.
           EXIT.
      * 
       END-PROGRAM.
            EXIT.
