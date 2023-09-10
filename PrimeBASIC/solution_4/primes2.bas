10 REM MICROSOFT BASIC SIEVE BY RZUCKERM - BIT VERSION
20 DIM A%(3125)
30 DIM BITS%(16)
40 DATA 1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, -32768
50 FOR I = 1 TO 16
60     READ BITS%(I)
70 NEXT I
80 N = 100000
90 NHALF = INT((N - 1) / 2)
100 NSQ% = INT((-1 + SQR(1 + 2 * NHALF)) / 2)
110 IW% = 1
120 IB% = 1
130 FOR I = 1 TO NSQ%
140     IF (A%(IW%) AND BITS%(IB%)) <> 0 THEN GOTO 290
150     JS = 2 * I * (I + 1)
160     JI = I * 2 + 1
170     JW% = INT((JS - 1) / 16) + 1
180     JB% = JS - (JW% - 1) * 16
190     KW% = INT(JI / 16)
200     KB% = JI - KW% * 16
210     FOR J = JS TO NHALF STEP JI
220         A%(JW%) = A%(JW%) OR BITS%(JB%)
230         JW% = JW% + KW%
240         JB% = JB% + KB%
250         IF JB% <= 16 THEN GOTO 280
260         JW% = JW% + 1
270         JB% = JB% - 16
280     NEXT J
290     IB% = IB% + 1
300     IF IB% <= 16 THEN GOTO 330
310     IB% = 1
320     IW% = IW% + 1
330 NEXT I
340 END
