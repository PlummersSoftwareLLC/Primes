:options.apply (str[]: ...args)
	@REM Usage: declare `options.{option}` variables, where `{option}` is the argument to be passed in
	@REM sets the `options` dictionary via arguments passed in
	@REM eg. `/workers:1` will run `set "options.workers=1"`
	for %%a in (%*) do (
		set "applied=0"
			
		for /f "tokens=* usebackq delims==" %%b in (`set options.`) do (
			for /f "tokens=1,2 delims=.=" %%c in ("%%b") do (
				set "arg=%%~a"
				if not "!arg:/%%d:=!" == "!arg!" (
					set "applied=1"
					set "%%c.%%d=!arg:/%%d:=!"
					set "%%c.%%d=!%%c.%%d:"=!"
				)
			)
		)

		if not "!applied!" == "1" (
			echo ignoring unrecognized option: %%a
		)
	)
	(exit /b)