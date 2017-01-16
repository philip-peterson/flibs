Checking compiler features
--------------------------

The collection of small programs in this directory was written to check
if the compiler of choice supports particular features or how they work
out:

- Features of the current Fortran standard (Fortran 2008).

- Run-time behaviour, such as what happens if the program reads a file
  with different line-endings than native to the platform.

- Extensions to the standards that may or may not be required for a
  particular program to function properly.

It is not exhaustive nor will it ever be. It should, however, give a
first start.


Running the checks
------------------

The checks are run, depending on the platform, via a batch file,
"chkfeatures.bat" or a shell script "chkfeatures":

- The program "buildscript" (as contained in the source file
  "buildscript.f90") prepares a simple batch file or shell script that
  builds and runs the actual checking programs

- Building is done via a batch file "compile.bat" or a shell script
  "compile", which is expected to build the program from the given
  source file. For instance, for gfortran, it contains the command:

  gfortran -o $1.exe $1.f90 $2

  where $1 is the name (without extension) of the source file to be
  compiled. The extension ".exe" is required (it makes it easy to
  distinguish the programs and to remove any leftovers at the start)

- The results are collected in a simple report "features.out". No
  interpretation beyond that contained in the source files is done.

- If a program cannot be compiled, then an appropriate message is
  printed instead.


Preparation
-----------

For all platforms: the batch file "compile.bat" and the shell script
"compile" are responsible for compiling and linking the program.

You will need to edit this file for the appropriate command for your
compiler. You may also need to set up the environment - make sure the
compiler is in the path, license information is available and so on.

On Linux:

Make sure that the files "chkfeatures" and "compile" have the execute
permission:

chmod +x chkfeatures compile

ought to do that.


Additional information
----------------------

The program "buildscript" takes the input file "buildscript.set" to set
up a small batch file/shell script that runs the various checking
programs. This means that you can add your own checking programs as
well.

The format of this file is very simple:

- A line starting with an at sign (@) should contain the name of the
  source file (without extension).

- It should be followed by one or more lines of text to be printed when
  the compilation fails as an aid to the user to interpret this failure.

- Then an empty line to separate each block.
