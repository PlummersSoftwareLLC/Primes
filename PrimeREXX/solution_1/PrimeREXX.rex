/* rexx */
/*
** Determine the prime numbers less than a number passed as a
** program parameter (sieveSize). The default sieveSize is 1,000,000
** Unlike many programming languages  with predefined data structure types,
** the stem variables used to store those odd numbers that are not prime
** use more memory.
**
** As noted in the original pull request, REXX is going to fall at or
** very near the bottom of the drag-race. This is due to three
** factors:
**   o  The submitted REXX is not compiled.
**   o  The code is therefore dependent upon the efficiency
**      of the Regina REXX interpreter implementation.
**   o  The possibility that there is performance to
**      be gained somewhere in this implementation.
*/
parse arg sieveSize outputMode
if sieveSize = 0 | sieveSize = 1 | sieveSize = 2 | sieveSize = 3 then
  do
    /* user did not supply the sieve size, use default 1,000,000 */
    outputMode = sieveSize
    sieveSize = 1000000
  end
else if datatype(sieveSize) \= "NUM" then
  sieveSize = 1000000

if datatype(outputMode) \= "NUM" then
  outputMode = 0 /* no output except the iteration count*/

sqRt = SquareRoot(sieveSize)

/*
** Note:
** o  The final iteration of the do while time("E") <= 5 loop may finish
**    outside the 5 second mark. So perhaps the final count is too high by
**    1 iteration, especially as the sieveSize gets larger.
*/
iterations = Sieve(sieveSize, sqRt)

/*
** Sieve iterations are complete, print the results
*/
call lineout 'stdout',"joss_REXX;"iterations";"primes._FINAL_ELAPSED";1;algorithm=base,bits=8,faithful=no"
select
  when outputMode = 1 then
    call PrintDetail sieveSize,printWidth
  when outputMode = 2 then
    call PrintSummary sieveSize
  when outputMode = 3 then
    do
      /* print summary last so you don't have to scroll to the top
      ** of the detail output to see the summary
      */
      call PrintDetail sieveSize,printWidth
      call PrintSummary sieveSize
    end
  otherwise
    nop
end /* select*/
return 0

/*
** Sieve:
**  Use the REXX stem 'primes.' to keep track of which numbers from 
**  3 to sieveSize are prime. Each iteration starts out with all 
**  numbers assumed prime (primes. = 1 initialization). Then as
**  the sieve eliminates multiples of the primes found, the 
**  relevant primes. entry is set to 0.
**  Notes:
**    o    The primes. stem is global (via expose)
**    o    1 and all even numbers could still be seen as prime
**         because the primes. entries are not set to 0. This
**         is handled by the PrintDetail() and PrintSummary()
**         subroutines which bypass 1 and the evens when
**         processing the primes. stem.
**    o    Since REXX is interpreted at run time (no JIT compiles)
**         a subroutine call starts at the top of the source and
**         reads the source until the subroutine is found. This
**         represents a minor performance loss (VERY minor). So
**         the timer start (t = time("R")) was moved to this
**         subroutine to bypass that loss and also allow for
**         these comments.
**    o    The code submitted to github.com/PlummersSoftwareLLC/Primes
**         had the Sieve() subroutine near the top of the code
**         to also remove the performance loss mentioned in
**         the last bullet. By moving the timer start to
**         the subroutine, the subroutine could be moved below
**         the main and perhaps be more readable.
**   There are no comments in Sieve() so the interpreter does not have
**   to process them.
*/
Sieve: procedure expose primes.
t = time("R")
parse arg sieveSize, sqRt
iterations = 0
do while time("E") <= 5
  primes. = 1
  do i = 3 to sqRt by 2
    if primes.i then
      do
        incr = i + i; strt = i * i
        do j = strt to sieveSize by incr
          primes.j = 0
        end j
      end
  end i
  iterations = iterations + 1
end
primes._FINAL_ELAPSED = time("E") 
return iterations

/*
** PrintDetail:
** Print all of the primes 2, 3 up to
** sieve size. Note that 2 is assumed prime
** the rest were calculated by Sieve.
*/
PrintDetail: procedure expose primes.
parse arg sieveSize
printWidth = length(sieveSize) + 1
line = right(2,printWidth)
/*
** linePrimesCount keeps track of how many primes are
** on a single line of output. Starts at 1 to
** accomodate the first output line containing
** "2".
*/
linePrimesCount = 1

do i = 3 to sieveSize by 2
  if primes.i then
   do
     line = line||right(i,printWidth," ")
     linePrimesCount = linePrimesCount + 1
     if linePrimesCount = 20 then
       do
         call lineout 'stdout', line
         line = ""
         linePrimesCount = 0
       end
   end
end i
if linePrimesCount > 0 then
  call lineout 'stdout', line
return

/*
** PrintSummary:
**   Print the count of primes less than powers of 10 up to 10,000,000
**   If sieveSize was specified, print the primes <= Sievesize and
**   stop.
*/
PrintSummary: procedure expose primes.
parse arg sieveSize
cnt = 1 /* accounts for 2 as prime */
do i = 3 to sieveSize by 2
  if primes.i then
    cnt = cnt + 1
  select
    when i = 99 & i <= sieveSize then
      call lineout 'stderr',cnt" primes less than 100"
    when i = 999 & i <= sieveSize then
      call lineout 'stderr', cnt" primes less than 1,000"
    when i = 9999 & i <= sieveSize then
      call lineout 'stderr', cnt" primes less than 10,000"
    when i = 99999 & i <= sieveSize then
      call lineout 'stderr', cnt" primes less than 100,000"
    when i = 999999 & i <= sieveSize then
      call lineout 'stderr', cnt" primes less than 1,000,000"
    when i = 9999999 & i <= sieveSize then
      call lineout 'stderr', cnt" primes less than 10,000,000"
    when i = sieveSize | i = sieveSize - 1 then
      call lineout 'stderr', cnt" primes less than or equal to "format(sieveSize)
    otherwise
      nop
  end /* select */
end i
return

/*
** SquareRoot function uses Newton's approximation method. There is no square
** root function in REXX.
*/
SquareRoot: procedure
parse arg number
guess = number / 2
guess2 = 0.5 * (number / guess + guess)
do while abs(guess2 - guess) > .0001
  guess = guess2
  guess2 = 0.5 * (number / guess + guess)
end /* while */
return guess2
/*
** Format:
**   Print a passed number with embedded commas.
**   The method was originally developed by Doug Nadel of
**   IBM.
*/
Format: procedure
parse arg num
temp1 = "abc,def,ghi,jkl,mno,pqr,stu,vwx"
temp2 = "abcdefghijklmnopqrstuvwx"
l = length(num) 
num = right(num,l+((3 - (l//3)) // 3))
return strip(space(translate(temp1,num,temp2),0),"T",",")
