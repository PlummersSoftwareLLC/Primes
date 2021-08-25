@echo off
setlocal enableDelayedExpansion

@REM misc optimization notes:
@REM * omitting per-function `setlocal` saves 1-2ms per 100 calls
@REM * some pseudo-for loops (*.n:) don't check if the initial condition is true, only the subsequent ones
@REM * (set "%1=%out%" && exit /b) is apparently faster than its line-separated equivalent
@REM * using if defined is faster than checking for var values
@REM * function calls are expensive

set "options.sieveSize=1000000"
@REM set "options.workers=%NUMBER_OF_PROCESSORS%"
set "options.workers=1"
set "options.pauseWorkersOnExit=0"
call :options.apply %*

set "PrimeResultCount[10]=4"
set "PrimeResultCount[100]=25"
set "PrimeResultCount[1000]=168"
set "PrimeResultCount[10000]=1229"
set "PrimeResultCount[100000]=9592"
set "PrimeResultCount[1000000]=78498"
set "PrimeResultCount[10000000]=664579"
set "PrimeResultCount[100000000]=576145"

set "WORKER_TITLE=primes#!random!"

set "resultsDirectory=%~dp0.results"
set "timekeeperSemaphorePath=%~dp0.timesup"

rmdir /s /q "%resultsDirectory%" 2>nul
mkdir "%resultsDirectory%" 2>nul

set "start=!time!"
set "expectedPasses=0"
set "activeWorkerCount=0"

call :spawnTimekeeper
call :spamSpawn
call :awaitExit
call :onFinish

endlocal
(exit /b)

:spamSpawn ()
	call :spawnWorkers %options.workers%

	set "lastPasses=0"
	:spamSpawn.0
		set "passes=0"
		pushd "%resultsDirectory%"
		for %%a in ("*") do (
			set /a "passes+=1"
		)
		popd

		if %passes% gtr !lastPasses! (
			set /a "finishedPassCount=%passes%-%lastPasses%"
			set /a "activeWorkerCount-=!finishedPassCount!"
			set /a "workersToSpawn=%options.workers%-!activeWorkerCount!"
			set "lastPasses=%passes%"
			call :spawnWorkers !workersToSpawn!
		)

		if not exist "%timekeeperSemaphorePath%" goto :spamSpawn.0

	(exit /b)

:awaitExit ()
	set "isFinished=0"
	:awaitExit.0
	for /f "tokens=2 usebackq" %%a in (`tasklist /nh /fi "windowtitle eq %WORKER_TITLE%"`) do (
		@REM get pid of all running workers
		for /f "delims=0123456789" %%b in ("%%a") do (
			@REM if not numeric, assume is error string & all of them have exited
			set "isFinished=1"
		)
	)
	ping localhost -n 2 >nul
	if not "!isFinished!" == "1" goto :awaitExit.0
	(exit /b)

:onFinish ()
	call :getDuration 0:0:0.0 0:0:5.0 longestDuration
	set "passes=0"
	pushd %resultsDirectory%
	for %%a in ("*") do (
		set /p data=<"%%~a"
		set "duration="

		call :getDuration %start% !data! duration

		if !duration.totalSecsInt! geq !longestDuration.totalSecsInt! (
			set "longestDuration=!duration!"
			set "longestDuration.hours=!duration.hours!"
			set "longestDuration.mins=!duration.mins!"
			set "longestDuration.secs=!duration.secs!"
			set "longestDuration.ms=!duration.ms!"
			set "longestDuration.totalSecs=!duration.totalSecs!"
			set "longestDuration.totalSecsInt=!duration.totalSecsInt!"
		)

		set /a "passes+=1"
	)
	popd
	call :printTime longestDuration !passes!
	(exit /b)

:spawnTimekeeper ()
	call :timekeeper.reset %timekeeperSemaphorePath%
	start "" cmd /c "^"timekeeper.bat^" /seconds:5 /semaphorePath:^^^"%timekeeperSemaphorePath%^^^""
	(exit /b)

:spawnWorkers (int: count)
	@REM if "!activeWorkerCount!" gtr 16 (
	@REM 	exit 1
	@REM )
	set /a "activeWorkerCount+=%1"
	for /l %%i in (1, 1, %1) do (
		start "" cmd /c "^"worker.bat^" /id:!expectedPasses! /noValidate:1 /title:%WORKER_TITLE% /sieveSize:%options.sieveSize% /resultsDirectory:^^^"%resultsDirectory%^^^""
		set /a "expectedPasses+=1"
	)
	(exit /b)

:printTime (obj: duration, int: passes)
	echo elapsed: !%1.hours!:!%1.mins!:!%1.secs!.!%1.ms! (!%1.totalSecs!.!%1.ms!s total)
	echo.
	echo ---
	echo batch;%2;!%1.totalSecs!.!%1.ms!;%options.workers%;algorithm=base,faithful=yes,bits=unknown
	(exit /b)

:getDuration (int: start, int: end, out obj: duration)
	for /f "tokens=1-4 delims=:.," %%a in ("%1") do set start_h=%%a&set /a start_m=100%%b %% 100&set /a start_s=100%%c %% 100&set /a start_ms=100%%d %% 100
	for /f "tokens=1-4 delims=:.," %%a in ("%2") do set end_h=%%a&set /a end_m=100%%b %% 100&set /a end_s=100%%c %% 100&set /a end_ms=100%%d %% 100

	set /a "hours=%end_h%-%start_h%"
	set /a "mins=%end_m%-%start_m%"
	set /a "secs=%end_s%-%start_s%"
	set /a "ms=%end_ms%-%start_ms%"
	if %ms% lss 0 set /a secs = %secs% - 1 & set /a ms = 100%ms%
	if %secs% lss 0 set /a mins = %mins% - 1 & set /a secs = 60%secs%
	if %mins% lss 0 set /a hours = %hours% - 1 & set /a mins = 60%mins%
	if %hours% lss 0 set /a hours = 24%hours%
	if 1%ms% lss 100 set ms=0%ms%
	set /a totalSecs=%hours%*3600+%mins%*60+%secs%
	set totalSecsInt=%totalSecs%
	set /a totalSecsInt+=0

	set "%3.hours=%hours%"
	set "%3.secs=%secs%"
	set "%3.mins=%mins%"
	set "%3.ms=%ms%"
	set "%3.totalSecs=%totalSecs%"
	set "%3.totalSecsInt=%totalSecsInt%"
	(exit /b)

:timekeeper.reset
timekeeper %*

:options.apply
options %*