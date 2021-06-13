       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRIMES.
       AUTHOR. FRANK VAN BAKEL.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  MAX_LIMIT                      PIC 9(7)   COMP VALUE 1000000.
       77  SHOW_RESULTS                   PIC 9(2)   COMP VALUE 0.
       77  MAX_TIME_SEC                   PIC 9(4)   COMP VALUE 5. 
       77  MAX_TIME_HS                    PIC 9(9)   COMP.
       77  DURATION-SEC                   PIC 9(1)V9 COMP-3.     
       77  PASSES                         PIC 9(5)   COMP.
       77  AVG                            PIC 9(5)v9 COMP-3.
       77  VAL-RES                        PIC X(5).   
       77  MAX_ROOT                       PIC 9(7)   COMP.
       77  MAX_ROOT_INDEX                 PIC 9(7)   COMP.
       77  START-AT                       PIC 9(7)   COMP.
       77  PRIME                          PIC 9(7)   COMP.
       77  PRIME-COUNT                    PIC 9(7)   COMP.
       77  FACTOR                         PIC 9(7)   COMP.
       77  STEP-SIZE                      PIC 9(7)   COMP.
       77  I                              PIC 9(7)   COMP.
       77  IS-EVEN                        PIC 9(1)v9 COMP-3.
       77  BIT_SIZE                       PIC 9(7)   COMP.
       01  BIT-ARRAY.
            03 FLAG PIC 1(1) 
               OCCURS 500000 TIMES
               INDEXED BY Z.
       01  WS-TIMES.
           03  WS-TIME                    PIC 9(8).
           03  WS-TIME-R REDEFINES WS-TIME.
               05  WS-TIME-H              PIC 9(2).
               05  WS-TIME-M              PIC 9(2).
               05  WS-TIME-S              PIC 9(2).
               05  WS-TIME-HS             PIC 9(2).
           03  NOW-HS                     PIC 9(9).    
           03  START-HS                   PIC 9(9).
           03  DURATION-HS                PIC 9(9).
       PROCEDURE DIVISION.
      * 
       START-UP.
           COMPUTE MAX_TIME_HS = 100 * MAX_TIME_SEC.
           MOVE ZERO TO PASSES.
           PERFORM GET-NOW THROUGH END-GET-NOW.
           MOVE NOW-HS TO START-HS.
           PERFORM UNTIL DURATION-HS >= MAX_TIME_HS
               PERFORM RUN_SIEVE THROUGH END-RUN_SIEVE
               ADD 1 TO PASSES
               PERFORM GET-NOW THROUGH END-GET-NOW
               COMPUTE DURATION-HS = NOW-HS - START-HS
           END-PERFORM.
           PERFORM PRINT_RESULTS THROUGH END-PRINT_RESULTS.

           STOP RUN.  
      *
       RUN_SIEVE.
           COMPUTE MAX_ROOT ROUNDED = (MAX_LIMIT ** .5).
           DIVIDE 2 INTO MAX_ROOT 
               GIVING MAX_ROOT_INDEX ROUNDED
               REMAINDER IS-EVEN.
           IF IS-EVEN = 0 THEN 
                   ADD -1 TO MAX_ROOT_INDEX.

           DIVIDE 2 INTO MAX_LIMIT
               GIVING BIT_SIZE ROUNDED
               REMAINDER IS-EVEN.
           IF IS-EVEN = 0 THEN 
                   ADD -1 TO BIT_SIZE.    
           PERFORM VARYING Z 
                   FROM 1 BY 1 
                   UNTIL Z>BIT_SIZE
               MOVE 1 TO FLAG (Z)
           END-PERFORM.
           MOVE 1 TO FACTOR.
           PERFORM UNTIL FACTOR > MAX_ROOT_INDEX
               IF FLAG (FACTOR) = 1 THEN
                   ADD FACTOR FACTOR 1 GIVING PRIME
                   ADD FACTOR FACTOR 1 GIVING STEP-SIZE
                   COMPUTE START-AT = (((PRIME * PRIME) - 1) / 2)
                   PERFORM VARYING Z 
                           FROM START-AT BY STEP-SIZE 
                           UNTIL Z>BIT_SIZE
                       MOVE 0 TO FLAG (Z)
                   END-PERFORM
               END-IF
               ADD 1 TO FACTOR
           END-PERFORM.
       END-RUN_SIEVE.
           EXIT.
      *
       PRINT_RESULTS.
           DIVIDE 100 INTO DURATION-HS GIVING DURATION-SEC.
           DIVIDE PASSES INTO DURATION-SEC GIVING AVG.
           PERFORM COUNT-PRIMES THROUGH END-COUNT-PRIMES.
           MOVE "True" TO VAL-RES.
           DISPLAY "Passes: ",PASSES,
                   ", Time: ",DURATION-SEC,
                   ", Avg: ",AVG,
                   "(sec/pass), Limit: ",MAX_LIMIT,
                   ", Count: ",PRIME-COUNT,
                   ", Valid: ",VAL-RES.
           DISPLAY " " *>Workaround to display empty new line
           DISPLAY "fvbakel_Cobol;",PASSES,
                   ";",DURATION-SEC,
                   ";1;algorithm=base,faithful=yes,bits=8".
       END-PRINT_RESULTS.
           EXIT.
      * 
       COUNT-PRIMES.
           MOVE 2 TO PRIME
           IF SHOW_RESULTS = 1 THEN
               DISPLAY "PRIME found=", PRIME. 
           MOVE 1 TO PRIME-COUNT.
           MOVE 1 TO I.
           PERFORM COUNT-PRIMES-LOOP UNTIL I > BIT_SIZE.
       END-COUNT-PRIMES.
           EXIT.
      * 
       COUNT-PRIMES-LOOP.
           IF FLAG (I) = 1 THEN
               ADD 1 TO PRIME-COUNT
               ADD I I 1 GIVING PRIME
               IF SHOW_RESULTS = 1 THEN
                   DISPLAY "PRIME found=", PRIME.
           ADD 1 TO I.
       END-COUNT-PRIMES-LOOP.
           EXIT.
      *
       GET-NOW.
           ACCEPT WS-TIME FROM TIME.
           COMPUTE NOW-HS = 
               (WS-TIME-H  * 360000) +
               (WS-TIME-M  *   6000) +
               (WS-TIME-S  *    100) +
               (WS-TIME-HS         ).
       END-GET-NOW.
           EXIT.       
       END-PROGRAM.
           EXIT.
