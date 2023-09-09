10 REM BASED ON REMARKABLE BASIC SIEVE BY DAVEPL
20 DIM A%(4998)
30 N = 10000
40 NHALF% = INT((N - 3) / 2)
50 NSQ% = INT((-1 + SQR(1 + 2 * NHALF%)) / 2)
60 FOR I = 1 TO NSQ%
70     IF A%(I) = 1 THEN GOTO 110
80     FOR J = 2 * INT(I * (I + 1)) TO NHALF% STEP I * 2 + 1
90        A%(J) = 1
100    NEXT J
110 NEXT I
120 COUNT% = 1
130 FOR I = 1 TO NHALF%
140     IF A%(I) = 0 THEN COUNT% = COUNT% + 1
150 NEXT I
160 EX% = -1
170 IF N = 10 THEN EX% = 4
180 IF N = 100 THEN EX% = 25
190 IF N = 1000 THEN EX% = 168
200 IF N = 10000 THEN EX %= 1229
210 RESULT$ = "INVALID"
220 IF COUNT% = EX% THEN RESULT$ = "VALID"
230 PRINT "NUMBER OF PRIMES UP TO"; N; "IS:"; COUNT%; "SHOULD BE: "; EX%; RESULT$
240 END
