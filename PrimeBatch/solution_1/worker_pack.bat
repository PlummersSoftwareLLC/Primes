@echo off
:: README.md has details on how this file is optimized
:: Packed-array worker: uses C[i] buckets with 30 bits each to reduce environment vars ~30x.
:: - Each bucket C[k] holds indices [30*k .. 30*k+29]
:: - Only odd numbers > 2 are considered; evens are trivially composite.
:: - Delayed expansion is required for bucket reads/writes.
setlocal enableDelayedExpansion

set "options.id=0"
set "options.title=_"
set "options.sieveSize=100000"
set "options.resultsDirectory=."
set "options.noValidate=0"
call :options.apply %*

title %options.title%
echo id: %options.id%
echo starting sieve... [%options.sieveSize%]

set "startTime=!time!"
call :runSieve %options.sieveSize%
set "endTime=!time!"
echo done

if "%options.noValidate%" == "0" (
	echo validating result...
	call :getPrimeCount %options.sieveSize% primeCount

	if not defined PrimeResultCount[%options.sieveSize%] (
        echo ERROR: No expected prime count defined for sieve size %options.sieveSize%.
        echo Please add expected count to main.bat
        pause
        exit /b 1
    )
	call :isExpectedPrimeCount !primeCount! %options.sieveSize% isExpectedCompositeIndexCount
	echo found: !primeCount!
	echo valid: !isExpectedCompositeIndexCount!	
	
	if "!isExpectedCompositeIndexCount!" == "0" (
		echo ERROR: algorithm found the wrong number of primes
		echo Here are the primes it found...
		echo Prime 2
		for /l %%i in (3, 2, %options.sieveSize%) do (
			call :isPrime %%i
			if !ERRORLEVEL! == 1 echo Prime %%i
		)
		echo Done listing primes
		pause
		exit /b 1
	)
)

: each worker will output a results file.  the name of the file is just
: the worker id.  inside is the end and start time for the main batch
: to compute durations from.
echo %endTime% %startTime% >"%options.resultsDirectory%/%options.id%"
endlocal
(exit /b)

:isExpectedPrimeCount (int: resultCount, int: size, out bool)
	echo expected: !PrimeResultCount[%2]!
	if %1 == !PrimeResultCount[%2]! (
		set "out=1"
	) else (
		set "out=0"
	)
	(set "%3=%out%" && exit /b)

:: Packed Sieve of Eratosthenes
:: - C[k] buckets initialized to 0
:: - Iterate odd f in [3..sqrt(N)]
:: - If f is prime, mark multiples from f*f to N, step 2f
:: using for loops to avoid slow goto n labels
:: note: we only iterate odd numbers.  Even numbers > 2
:: are always composite and trivially not a prime.  
:runSieve (int: size)
	call :sqrt %1 q
    set /a bound=%1/30+1
	for /l %%i in (0,1,!bound!) do set "C[%%i]=0"

	for /l %%f in (3,2,!q!) do (
		call :isPrime %%f
		if !ERRORLEVEL! == 1  (
			set /a "step=%%f*2"
			set /a "start=%%f*%%f"
			for /l %%n in (!start!, !step!, %1) do (
				call :markComposite %%n
			)
		)
	)
	goto :eof

:markComposite (int: num)
    set /a "varindex=%1/30"
    set /a "boffset=%1%%30"
    set /a "mask=1<<boffset"
    set /a "C[%varindex%]=!C[%varindex%]!|mask"
    exit /b

:: Prime test using packed buckets
:: Exit code: 1 = prime, 0 = composite
:: only call for values that are odd and above 2
:isPrime (int: index, out: ERRORLEVEL)
    set /a "varindex=%1/30"
    set /a "boffset=%1%%30"
    set /a "mask=1<<boffset"
    set /a "currval=!C[%varindex%]!"
    set /a "test=currval&mask"
    if !test! neq 0 (exit /b 0)
    exit /b 1

:: Integer square root using Newton's method
:: Precondition: %1 >= 3
:: Usage: call :sqrtNewton MyVal ResultVar
:sqrt (int, out int)
	set /A "rt=%1/200+2, rt=(%1/rt+rt)/2, rt=(%1/rt+rt)/2, rt=(%1/rt+rt)/2, rt=(%1/rt+rt)/2, rt=(%1/rt+rt)/2"
	:: Correction step (floor)
	set /A "sqr=rt*rt"
	if %sqr% gtr %1 set /A "rt-=1"
	(set "%2=%rt%" && exit /b)

:: 2 is implicitly a prime, we check our array for all
:: primes greater than 2.
:getPrimeCount (int size, out int)
	if %1 geq 2 (
		set "count=1"
	) else (
		set "count=0"
	)
	
	for /l %%i in (3, 2, %1) do (
		call :isPrime %%i 
		if !ERRORLEVEL! == 1 (set /a "count+=1")
	)
	(set "%2=%count%" && exit /b)

:options.apply
options %*
