@echo off
rem rundiag.bat --
rem     Run the diagnostics program (uses compile.bat to run the compiler)
rem

if not exist compdiag.complete goto loop
    if exist check.out          del check.out
    if exist compdiag.count     del compdiag.count
    if exist compdiag.test      del compdiag.test
    if exist compdiag.complete  del compdiag.complete
    if exist compdiag.log       del compdiag.log
    if exist compdiag.summary   del compdiag.summary
    if exist compdiag.score     del compdiag.score

:loop
    compdiag
    call compile check.f90 1>check.out 2>&1
    if errorlevel 1 copy rundiag.bat compdiag.error >nul
    compdiag
    if exist compdiag.complete goto end
    goto loop

:end
