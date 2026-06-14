@echo off
setlocal enableDelayedExpansion

@REM misc optimization notes:
@REM * omitting per-function `setlocal` saves 1-2ms per 100 calls
@REM * some pseudo-for loops (*.n:) don't check if the initial condition is true, only the subsequent ones
@REM * (set "%1=%out%" && exit /b) is apparently faster than its line-separated equivalent
@REM * using if defined is faster than checking for var values
@REM * function calls are expensive
@REM * scanning the env var space for a large set of long similar variable names is slow.  So use a short name to reduce comparison ops.

set "options.sieveSize=1000000"
set "options.noValidate=1"
set "options.workers=1"
set "options.pauseWorkersOnExit=0"
call :options.apply %*

set "PrimeResultCount[10]=4"
set "PrimeResultCount[100]=25"
set "PrimeResultCount[1000]=168"
set "PrimeResultCount[5000]=669"
set "PrimeResultCount[10000]=1229"
set "PrimeResultCount[100000]=9592"
set "PrimeResultCount[1000000]=78498"
set "PrimeResultCount[10000000]=664579"
set "PrimeResultCount[100000000]=576145"

REM Clamp workers to free CPUs
set /a "cpuFreeCount=%NUMBER_OF_PROCESSORS%-1"
set "requestedWorkers=%options.workers%"
if %requestedWorkers% gtr %cpuFreeCount% (
    echo WARNING: Requested %requestedWorkers% workers but only %cpuFreeCount% CPUs available
    echo Limiting to %cpuFreeCount% workers
    set "options.workers=%cpuFreeCount%"
)

REM all workers we spawn will have same title so we can find them later.
set "WORKER_TITLE=Primes_Worker_!random!"

set "resultsDirectory=%~dp0.results"
set "timekeeperSemaphorePath=%~dp0.timesup"

REM remove any previous run results
rmdir /s /q "%resultsDirectory%" 2>nul
mkdir "%resultsDirectory%" 2>nul

set "start=!time!"
set "expectedPasses=0"
set "activeWorkerCount=0"

set startFramework=!time!
call :spamSpawn
call :awaitExit
set endFramework=!time!
call :onFinish

endlocal
(exit /b)

:spamSpawn ()
	call :spawnWorkers %options.workers%
	goto :eof

:: poll tasklist for running workers by title & wait til none remain
:: we get pid of all running workers, if not numeric, assume is error 
:: string & all of them have exited
:: Note: on start we wait 5 seconds so all workers had a chance to 
:: start and can be found by tasklist
:awaitExit ()
    echo ---
    ping localhost -n 5 > nul
	set "isFinished=0"
	:awaitExit.0
	for /f "tokens=2" %%a in ('tasklist /nh /fi "windowtitle eq %WORKER_TITLE%"') do (
		for /f "delims=0123456789" %%b in ("%%a") do (
			set "isFinished=1"
		)
	)
	echo [heartbeat] %time% - worker processes still running
	ping localhost -n 5 >nul
	if not "!isFinished!" == "1" goto :awaitExit.0
	goto :eof

:: Note: our min duration is hundredth of a second which is pretty noisy considering
:: the overhead of spawning a new process etc.
:: Note: when we open a worker result file (the filename is the worker id) 
::  it will contain a single line which looks like "23:58:45.31 23:58:45.30"
:onFinish ()
    echo ---
	call :getDuration 0:0:0.00 0:0:0.01 longestDuration
	set "passes=0"
	pushd %resultsDirectory%
	for %%a in ("*") do (
		set /p line=<"%%~a"
		for /f "tokens=1,2" %%x in ("!line!") do (
			set "end=%%x"
			set "start=%%y"
		)
		set "duration="
		call :getDuration !start! !end! duration

		if !duration.totalCs! geq !longestDuration.totalCs! (
			set "longestDuration=!duration!"
			set "longestDuration.hours=!duration.hours!"
			set "longestDuration.mins=!duration.mins!"
			set "longestDuration.secs=!duration.secs!"
			set "longestDuration.cs=!duration.cs!"
			set "longestDuration.totalCs=!duration.totalCs!"
			set "longestDuration.totalSecsInt=!duration.totalSecsInt!"
		)
		call :printWorker duration %%a
		set /a "passes+=1"
	)
	popd
	call :getDuration !startFramework! !endFramework! frameworkDuration
	echo ---
	call :printTime longestDuration !passes! frameworkDuration
	goto :eof

:spawnWorkers (int: count)
    set workBat=worker.bat
	echo Spawning %1 workers [!workBat!]
	set /a "activeWorkerCount+=%1"
	:: workers go on CPU1 and up.  framework and all kinds of stuff on cpu0
	set /a "mask=2"
	for /l %%i in (1, 1, %1) do (
		start "" /affinity !mask! cmd /c "^"!workBat!^" /id:!expectedPasses! /noValidate:%options.noValidate% /title:%WORKER_TITLE% /sieveSize:%options.sieveSize% /resultsDirectory:^^^"%resultsDirectory%^^^"" 
		set /a "expectedPasses+=1"
		set /a "mask*=2"
	)
	goto :eof

:printTime (obj: duration, int: passes, obj: framework)
    set /a secsPart=!%1.totalCs! / 100
	set /a csPart=!%1.totalCs! %% 100
	if !csPart! lss 10 set csPart=0!csPart!

	set "csDisplay=!%1.cs!"
	if !csDisplay! lss 10 set csDisplay=0!csDisplay!

	echo longest: !%1.hours!:!%1.mins!:!%1.secs!.!csDisplay! (!secsPart!.!csPart!s total)

    set /a secsPartFr=!%3.totalCs! / 100
	set /a csPartFr=!%3.totalCs! %% 100
	if !csPartFr! lss 10 set csPartFr=0!csPartFr!

    set "csDisplayFr=!%3.cs!"
    if !csDisplayFr! lss 10 set csDisplayFr=0!csDisplayFr!

	echo framework: !%3.hours!:!%3.mins!:!%3.secs!.!csDisplayFr! (!secsPartFr!.!csPartFr!s total)
	echo.
	echo ---
	echo batch;%2;!secsPart!.!csPart!;%options.workers%;algorithm=base,faithful=yes
	goto :eof

:printWorker (obj: duration, int: worker_number)
    set /a secsPart=!%1.totalCs! / 100
	set /a csPart=!%1.totalCs! %% 100
	if !csPart! lss 10 set csPart=0!csPart!

	set "csDisplay=!%1.cs!"
	if !csDisplay! lss 10 set csDisplay=0!csDisplay!

	echo worker %2 elapsed: !%1.hours!:!%1.mins!:!%1.secs!.!csDisplay! (!secsPart!.!csPart!s total)
	goto :eof

:getDuration (int: start, int: end, out obj: duration)
	for /f "tokens=1-4 delims=:.," %%a in ("%1") do set start_h=%%a&set /a start_m=100%%b %% 100&set /a start_s=100%%c %% 100&set /a start_cs=100%%d %% 100
	for /f "tokens=1-4 delims=:.," %%a in ("%2") do set end_h=%%a&set /a end_m=100%%b %% 100&set /a end_s=100%%c %% 100&set /a end_cs=100%%d %% 100

	set /a "hours=%end_h%-%start_h%"
	set /a "mins=%end_m%-%start_m%"
	set /a "secs=%end_s%-%start_s%"
	set /a "cs=%end_cs%-%start_cs%"

	if %cs% lss 0 set /a secs = %secs% - 1 & set /a cs = 100%cs%
	if %secs% lss 0 set /a mins = %mins% - 1 & set /a secs = 60%secs%
	if %mins% lss 0 set /a hours = %hours% - 1 & set /a mins = 60%mins%
	if %hours% lss 0 set /a hours = 24%hours%

	set /a totalCs=%hours%*360000 + %mins%*6000 + %secs%*100 + %cs%
	set /a totalSecsInt=%totalCs% / 100
	set /a totalSecsInt+=0

	set "%3.hours=%hours%"
	set "%3.secs=%secs%"
	set "%3.mins=%mins%"
	set "%3.cs=%cs%"
	set "%3.totalSecsInt=%totalSecsInt%"
	set "%3.totalCs=%totalCs%"
	goto :eof

:options.apply
options %*