.JOB SIEVE ABORT=CANCEL
.NOLOG
* Sieve of Eratosthenes by ren14500. Public domain.
*
* Variable assignments:
*   0 = prime flags 0-32.
*   1 = prime flags 33-65.
*   2 = prime flags 66-98.
*   3 = working variable WORD.
*   4 = working variable FACTOR.
*   5 = working variable POW2.
*   6 = working variable.
*   7 = working variable OFFSET.
*   8 = working variable J/COUNT.
*   9 = working variable I.
*   UPSI = INDEX.
*
* Flags hold 33 bits per variable. Max prime is 98. Square root of max prime
* is 9.
*
* Print the start time.
.TIME
/ START
.NOTIME
*
* Set all odd bits to 1 to assume prime. Non-primes will get zeroed later.
* 2863311530 = 0x0AAAAAAAA, 5726623061 = 0x155555555. We have 33 bits per var.
.SETA 0=2863311530, 1=5726623061, 2=2863311530
*
* Outer loop to search for the next prime and clear multiples thereof.
.SETA 4=3
=OUTER
  *
  * Find the next prime.
  .SETA 9=#4
=FINDLOOP
  * Calculate which word has the factor.
  .SETA UPSI=#9/33
  * And which bit within that word.
  .SETA 7=#9-(#U*33)
  * Calculate the power of 2 to extract the bit we want.
  .SETA 8=0, 5=1
  .SKIP TO POW1END IF 0 EQ #7
=POW1
    .SETA 5=#5*2, 8=#8+1
    .SKIPR TO POW1 IF #8 LT #7
=POW1END
  * Get the word containing our bit.
  .SKIP TO FLW#U
=FLW0
  .SETA 3=#0
  .SKIP TO FLWEND
=FLW1
  .SETA 3=#1
  .SKIP TO FLWEND
=FLW2
  .SETA 3=#2
=FLWEND
  * Divide to get our bit in the 0 position.
  .SETA 8=#3/#5
  * Divide by two.
  .SETA 6=#8/2
  * Calculate the remainder, which is our bit.
  .SETA 8=#8-(#6*2)
  * If the bit is not set, it's not prime, so check for the next.
  .SKIP TO FLCHECK IF 0 EQ #8
  * Set the prime and break out of the loop.
  .SETA 4=#9
  .SKIP TO FLEND
=FLCHECK
  * Next possible prime.
  .SETA 9=#9+2
  .SKIPR TO FINDLOOP IF 98 GT #9
=FLEND
  *
  * Clear multiples of this prime. Start at the factor squared.
  .SETA 9=#4*#4
  .SKIP TO OCHECK IF 98 LT #9
=CLRLOOP
  * Calculate which word has the factor.
  .SETA UPSI=#9/33
  * And which bit within that word.
  .SETA 7=#9-(#U*33)
  * Calculate the power of 2 to extract the bit we want.
  .SETA 8=0, 5=1
  .SKIP TO POW2END IF 0 EQ #7
=POW2
    .SETA 5=#5*2, 8=#8+1
    .SKIPR TO POW2 IF #8 LT #7
=POW2END
  * Get the word containing our bit.
  .SKIP TO CLW#U
=CLW0
  .SETA 3=#0
  .SKIP TO CLWEND
=CLW1
  .SETA 3=#1
  .SKIP TO CLWEND
=CLW2
  .SETA 3=#2
=CLWEND
  * Divide to get our bit in the 0 position.
  .SETA 8=#3/#5
  * Divide by two.
  .SETA 6=#8/2
  * Calculate the remainder, which is our bit.
  .SETA 8=#8-(#6*2)
  * If the bit is not set, go to next.
  .SKIP TO CLCHECK IF 0 EQ #8
  * Clear the bit.
  .SKIP TO CB#U
=CB0
  .SETA 0=#3-#5
  .SKIP TO CLCHECK
=CB1
  .SETA 1=#3-#5
  .SKIP TO CLCHECK
=CB2
  .SETA 2=#3-#5
=CLCHECK
  * Next multiple.
  .SETA 9=#9+(#4*2)
  .SKIPR TO CLRLOOP IF 98 GT #9
*
* Skip to the next possible factor.
=OCHECK
.SETA 4=#4+2
.SKIPR TO OUTER IF 9 GE #4
*
* Print the calculation time.
.TIME
/ CALCULATED
.NOTIME
* Skip past the first prime (2).
/ 2
.SETA 8=1
.SETA 0=#0/8
*
* Print primes and get the count.
.SETA 5=3
.SETA 4=3
=P0LOOP
.SETA 6=#0/2
.SETA 3=#0-(#6*2)
.SETA 0=#6
.SKIP TO P0CHECK IF 0 EQ #3
.SETA 8=#8+1
/ #5
=P0CHECK
.SETA 5=#5+1
.SETA 4=#4+1
.SKIPR TO P0LOOP IF 33 GT #4
.SETA 4=0
=P1LOOP
.SETA 6=#1/2
.SETA 3=#1-(#6*2)
.SETA 1=#6
.SKIP TO P1CHECK IF 0 EQ #3
.SETA 8=#8+1
/ #5
=P1CHECK
.SETA 5=#5+1
.SETA 4=#4+1
.SKIPR TO P1LOOP IF 33 GT #4
.SETA 4=0
=P2LOOP
.SETA 6=#2/2
.SETA 3=#2-(#6*2)
.SETA 2=#6
.SKIP TO P2CHECK IF 0 EQ #3
.SETA 8=#8+1
/ #5
=P2CHECK
.SETA 5=#5+1
.SETA 4=#4+1
.SKIPR TO P2LOOP IF 33 GT #4
/ COUNT: #8
*
* Print the end time.
.TIME
/ FINISH
.NOTIME
.END
S.CON

