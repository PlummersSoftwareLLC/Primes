/* rexx */
signal Main

Sieve:
count = 0
do while time("E") <= 5
  primes. = 1
  do i = 3 to sqRt by 2
    if primes.i then
      do
        incr = i + i; strt = i * i
        do j = strt to maxVal by incr
          primes.j = 0
        end j
      end
  end i
  count = count + 1
end
return

/*
** Determine the prime numbers less than a number passed as a
** program parameter (maxVal). The default maxVal is 1,000,000
** Unlike many programming languages  with predefined data structure type,
** the stem variables used to store those odd numbers that are not prime
** use more memory.
**
*/
Main:
parse arg maxVal printSw
if maxVal = 0 | maxVal = 1 | maxVal = 2 | maxVal = 3 then
  do
    printSw = maxVal
    maxVal = 1000000
  end
else if datatype(maxVal) \= "NUM" then
  maxVal = 1000000

if datatype(printSw) \= "NUM" then
  printSw = 0 /* no output except the iteration count*/
printWidth = length(maxVal) + 1

/*
** SquareRoot function uses Newton's approximation method. There is no square
** root function in REXX.
*/
sqRt = SquareRoot(maxVal)

/*
** Note:
** o  The final iteration of the do while time("E") <= 5 loop may finish
**    outside the 5 second mark. So perhaps the final count is too high by
**    1 iteration, especially as the maxVal gets larger.
**
**   Actual Sieve logic starts here, no comments in Sieve()
***/
t = time("R")
call Sieve
t = time("E") /* close enough for algorithm run time */

/*
** Sieve iterations are complete, print the results
*/
call lineout 'stdout',"joss_REXX;"count";"t";1;algorithm=base,bits=8,faithful=no"
select
  when printSw = 1 then
    call PrintDetail maxVal,printWidth
  when printSw = 2 then
    call PrintSummary maxVal
  when printSw = 3 then
    do
      call PrintSummary maxVal
      call PrintDetail maxVal,printWidth
    end
  otherwise
    nop
end /* select*/
return 0


PrintDetail: procedure expose primes.
parse arg maxVal,printWidth
line = right(2,printWidth)
digitCount = 1


do i = 3 to maxVal by 2
  if primes.i = 1 then
   do
     line = line||right(i,printWidth," ")
     digitCount = digitCount + 1
     if digitCount = 20 then
       do
         call lineout 'stdout', line
         line = ""
         digitCount = 0
       end
   end
end i
if digitCount > 0 then
  call lineout 'stdout', line
return


PrintSummary: procedure expose primes.
parse arg maxVal
cnt = 1 /* accounts for 2 as prime */
do i = 3 to maxVal by 2
  if primes.i then
    cnt = cnt + 1
  select
    when i = 99 & i <= maxVal then
      call lineout 'stderr',cnt" primes less than 100"
    when i = 999 & i <= maxVal then
      call lineout 'stderr', cnt" primes less than 1,000"
    when i = 9999 & i <= maxVal then
      call lineout 'stderr', cnt" primes less than 10,000"
    when i = 99999 & i <= maxVal then
      call lineout 'stderr', cnt" primes less than 100,000"
    when i = 999999 & i <= maxVal then
      call lineout 'stderr', cnt" primes less than 1,000,000"
    when i = 9999999 & i <= maxVal then
      call lineout 'stderr', cnt" primes less than 10,000,000"
    when i = maxVal | i = maxVal - 1 then
      call lineout 'stderr', cnt" primes less than or equal to "format(maxVal)
    otherwise
      nop
  end /* select */
end i
return

SquareRoot: procedure
parse arg number
guess = number / 2
guess2 = .5 * (number / guess + guess)
do while abs(guess2 - guess) > .0001
  guess = guess2
  guess2 = .5 * (number / guess + guess)
end /* while */
return guess2

Format: procedure
parse arg num
temp1 = "abc,def,ghi,jkl,mno,pqr,stu,vwx"
temp2 = "abcdefghijklmnopqrstuvwx"
l = length(num)
l3 = (3 - (l//3)) // 3
i = right(num,l+l3)
return strip(space(translate(temp1,i,temp2),0),"T",",")
