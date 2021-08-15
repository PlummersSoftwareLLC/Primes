@echo off
setlocal enableDelayedExpansion

set "options.id=0"
set "options.title=_"
set "options.sieveSize=100000"
set "options.resultsDirectory=."
set "options.noValidate=0"
call :options.apply %*

title %options.title%

echo starting sieve...

call :runSieve %options.sieveSize%
set "end=!time!"

if "%options.noValidate%" == "0" (
	echo validating result...

	call :getPrimeCount %sieveSize% primeCount
	call :isExpectedPrimeCount !primeCount! %options.sieveSize% isExpectedCompositeIndexCount

	echo count: !primeCount!
	echo valid: !isExpectedCompositeIndexCount!

	if "!isExpectedCompositeIndexCount!" == "0" (
		pause
		exit 1
	)
)

echo %end%>"%options.resultsDirectory%/%options.id%"

endlocal
(exit /b)

:isExpectedPrimeCount (int: resultCount, int: size, out bool)
	if %1 == !PrimeResultCount[%2]! (
		set "out=1"
	) else (
		set "out=0"
	)
	(set "%3=%out%" && exit /b)

:runSieve (int: size)
	call :sqrt %1 q

	set /a "factor=3"
	:runSieve.0

		set /a "num=%factor%"
		:runSieve.1
			@REM call :isPrime %num% isNumPrime
			set /a "isOdd=%num%&1"
			if %isOdd% == 1 if not defined composites[%num%] set "factor=%num%" & goto :runSieve.1e
			set /a "num+=2"
			if %num% lss %1 goto :runSieve.1
			:runSieve.1e

		echo factor: %factor%

		set /a "num=%factor%*%factor%"
		set /a "factorSqr=%factor%*2"
		:runSieve.2
			set "composites[%num%]=1"
			set /a "num+=%factorSqr%"
			if %num% lss %1 goto :runSieve.2

		set /a "factor+=2"
		if %factor% leq !q! goto :runSieve.0
	(exit /b)

:getPrimeCount (int size, out int)
	if %1 geq 2 (
		set "count=1"
	) else (
		set "count=0"
	)
	
	for /l %%i in (3, 1, %1) do (
		call :isPrime %%i isPrime
		if !isPrime! == 1 set /a "count+=1"
	)
	(set "%2=%count%" && exit /b)

:isPrime (int: index, out bool)
	set /a "isOdd=%1&1"
	if %isOdd% == 0 (set "%2=0" && exit /b)
	if !composites[%1]! == 1 (set "%2=0" && exit /b)
	(set "%2=1" && exit /b)

:sqrt (int, out int)
    set "root=1"
	set "sqr=1"
	:sqrt.0
		if %sqr% lss %1 (
			set /a "root+=1"
			set /a "sqr=!root!*!root!"
			goto :sqrt.0
		)
	(set "%2=%root%" && exit /b)

:options.apply
options %*