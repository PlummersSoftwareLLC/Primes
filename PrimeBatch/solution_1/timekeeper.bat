@echo off
setlocal enableDelayedExpansion

set "options.seconds=5"
set "options.semaphorePath=_"
call :options.apply %*

title timekeeper

call :timekeeper.reset "%options.semaphorePath%"
call :timekeeper.time %options.seconds% "%options.semaphorePath%"

(exit /b)

:timekeeper.time (int: seconds, str semaphorePath)
	ping localhost -n %1 >nul
	echo. > "%~2"
	(exit /b)

:timekeeper.reset (str semaphorePath)
	del "%~1" >nul 2>&1
	(exit /b)

:options.apply
options %*