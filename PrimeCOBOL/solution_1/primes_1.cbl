       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMES.
       AUTHOR. FRANK VAN BAKEL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  PRIME                          PIC 9(5) COMP.
       77  PRIME-COUNT                    PIC 9(5) COMP.
       77  I                              PIC 9(4) COMP.
       77  K                              PIC 9(5) COMP.
       01  BIT-ARRAY.
            03 FLAG OCCURS 100 TIMES       PIC 9 COMP.
       PROCEDURE DIVISION.
       START-UP.
            DISPLAY "TEN ITERATIONS".
            PERFORM SIEVE THROUGH SIEVE-END.
            DISPLAY "PRIMES FOUND: ", PRIME-COUNT.
            STOP RUN.
       SIEVE.
            MOVE ZERO TO PRIME-COUNT.
            MOVE 1 TO I.
            PERFORM INIT-BITS 100 TIMES.
            MOVE 1 TO I.
            PERFORM SCAN-FOR-PRIMES THROUGH END-SCAN-FOR-PRIMES
                 100 TIMES.
       SIEVE-END.
            EXIT.
       INIT-BITS.
            MOVE 1 TO FLAG (I).
            ADD 1 TO I.
       END-INIT-BITS.
            EXIT.
       SCAN-FOR-PRIMES.
            IF FLAG (I) = 0
                 THEN
                      GO TO NOT-PRIME.
            ADD I I 1 GIVING PRIME.
            DISPLAY PRIME.
            ADD I PRIME GIVING K.
            DISPLAY "K=", K.
            PERFORM STRIKOUT UNTIL K > 100.
            ADD 1 TO PRIME-COUNT.
       NOT-PRIME.
            ADD 1 TO I.
       END-SCAN-FOR-PRIMES.
            EXIT.
       STRIKOUT.
            MOVE 0 TO FLAG (K).
            ADD PRIME TO K.
       END-PROGRAM.
            EXIT.
