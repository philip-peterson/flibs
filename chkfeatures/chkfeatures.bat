@echo off
rem
rem Batch file to run the feature checks:
rem a series of small programs that use particular features of the Fortran standard
rem or certain run-time features that may or may not work with a particular compiler
rem
rem Note:
rem This is the batchfile version. It requires a batchfile "compile.bat" that
rem builds a program from a given Fortran source file. For instance:
rem
rem compile.bat:
rem gfortran -o %1.exe %1.f90 %2
rem
rem where %1 is the name of the source file (no extension) and %2 any additional flags
rem
rem Note: the executable program should have the extension .exe to make it easy to clean
rem up the work directory
rem
rem Steps:
rem - Clean up (no .exe files)
rem - Build the auxiliary program that writes the dedicated batchfile and shell script
rem   to build and run the various feature checking programs
rem - Run this program
rem - Run the batchfile or shell script
rem

echo Start checking procedure ...

if /%1 == / goto nooptions
    echo Note:
    echo Compiler options: %1 %2 %3 %4 %5 %6 %7 %8
    goto run
:nooptions
    echo Note:
    echo No compiler options

:run
del *.exe

call compile buildscript

buildscript.exe

call runfeatures >features.out 2>&1

echo Done - results in features.out
