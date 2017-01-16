#!/bin/sh
# rundiag.bat --
#     Run the diagnostics program (uses compile.bat to run the compiler)
#

if [-f compdiag.complete]; then
    for f in (check.out compdiag.count compdiag.test compdiag.complete \
        compdiag.log compdiag.summary compdiag.score); do
    if [-f $f]; then
        rm -f $f
    fi
fi

while [! -f compdiag.complete] ;do
    compdiag
    compile check.f90 1>check.out 2>&1
    rc=$?
    if [$rc != 0]; then
        touch compdiag.error
    fi
    compdiag
done
