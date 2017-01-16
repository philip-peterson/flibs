#******************************************************************************#
#                    Fortran-Compatible Regular Expressions                    #
#------------------------------------------------------------------------------#
#                                                                              #
#  Fortran bindings for the PCRE library.                                      #
#                                                                              #
#  Written by Paul Fossati, <paul.fossati@gmail.com>                           #
#  Copyright (c) 2014 Paul Fossati                                             #
#                                                                              #
#  PCRE library written by Philip Hazel,                                       #
#  Copyright (c) 1997-2014 University of Cambridge                             #
#  available at http://www.pcre.org                                            #
#------------------------------------------------------------------------------#
# Redistribution and use in source and binary forms, with or without           #
# modification, are permitted provided that the following conditions are met:  #
#                                                                              #
#     * Redistributions of source code must retain the above copyright notice, #
#       this list of conditions and the following disclaimer.                  #
#                                                                              #
#     * Redistributions in binary form must reproduce the above copyright      #
#       notice, this list of conditions and the following disclaimer in the    #
#       documentation and/or other materials provided with the distribution.   #
#                                                                              #
#     * The name of the author may not be used to endorse or promote products  #
#      derived from this software without specific prior written permission    #
#      from the author.                                                        #
#                                                                              #
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"  #
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE    #
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE   #
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE     #
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR          #
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF         #
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS     #
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN      #
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      #
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE   #
# POSSIBILITY OF SUCH DAMAGE.                                                  #
#******************************************************************************#

# Select the compilers of your choice
#
# Note:
# The macros OBJ and EXE are actually dependent on both the compiler and the
# operating system. Choose the ones that are suitable.
#
# Set the correct value for all macros
#
# GNU Compilers
# Fortran compiler
#FC= gfortran
# C compiler
#CC= gcc
#OBJ=.o
#EXE=
#Or:
#EXE=.exe
#CFLAGS= -g -pg -O0  -fsanitize=address
#FCFLAGS= $(CFLAGS)
#LNKOUT=-o
#PCRE_SUFFIX=
#PCRELIB= -L../pcre -l
#PCREINCLUDE= -I../pcre/include
#FUNIT_SUFFIX=../../src/funit/

# Intel Fortran and MSVC/C++ or Intel C
#FC= ifort
# C compiler
#CC= cl
#CC= icc
#OBJ=.obj
#EXE=.exe
#CFLAGS=
#FCFLAGS= $(CFLAGS) /link /nodefaultlib:msvcrtd
#LNKOUT=-exe:
#PCRE_SUFFIX=d.lib
#PCRELIB= ..\pcre\\
#PCREINCLUDE= -I..\pcre
#FUNIT_SUFFIX=..\\..\\src\\funit\\


# Selected: Intel Fortran + MSVC/C++
FC= ifort
CC= cl
OBJ=.obj
EXE=.exe
CFLAGS=
FCFLAGS= $(CFLAGS) /link /nodefaultlib:msvcrtd
LNKOUT=-exe:
FUNIT_SUFFIX=..\\..\\src\\funit\\
PCRE_SUFFIX=d.lib
PCRELIB= .\\
PCREINCLUDE= -Iinclude

all: example$(EXE) wordcount$(EXE)

#
# Unit tests
#
test: test_regex$(EXE)
        echo > ftnunit.run
        ./test_regex

test_regex$(EXE): mod_regex$(OBJ) fcre$(OBJ) test_regex$(OBJ) pcre_wrappers$(OBJ) ftnunit$(OBJ)
	$(FC) $(LNKOUT)test_regex$(EXE)  fcre$(OBJ) mod_regex$(OBJ) test_regex$(OBJ) pcre_wrappers$(OBJ) ftnunit$(OBJ) ftnunit_hooks$(OBJ) $(PCRELIB)pcre$(PCRE_SUFFIX) $(FCFLAGS)

test_regex$(OBJ): test_regex.f90 mod_regex$(OBJ) ftnunit$(OBJ) ftnunit_hooks$(OBJ)
	$(FC) -c test_regex.f90 $(FCFLAGS)

ftnunit$(OBJ): $(FUNIT_SUFFIX)ftnunit.f90 ftnunit_hooks$(OBJ)
	$(FC) -c $(FUNIT_SUFFIX)ftnunit.f90 $(FCFLAGS)

ftnunit_hooks$(OBJ): $(FUNIT_SUFFIX)ftnunit_hooks.f90
	$(FC) -c $(FUNIT_SUFFIX)ftnunit_hooks.f90 $(FCFLAGS)

#
# Sample program 1
#
wordcount$(EXE): fcre$(OBJ) mod_regex$(OBJ) wordcount$(OBJ) pcre_wrappers$(OBJ)
	$(FC) $(LNKOUT)wordcount$(EXE)  fcre$(OBJ) mod_regex$(OBJ) wordcount$(OBJ) pcre_wrappers$(OBJ) $(PCRELIB)pcre$(PCRE_SUFFIX) $(FCFLAGS)

wordcount$(OBJ): wordcount.f90 mod_regex$(OBJ)
	$(FC) -c wordcount.f90 $(FCFLAGS)

#
# Sample program 2
#
example$(EXE): mod_regex$(OBJ) fcre$(OBJ) example$(OBJ) pcre_wrappers$(OBJ)
	$(FC) $(LNKOUT)example$(EXE)  fcre$(OBJ) mod_regex$(OBJ) example$(OBJ) pcre_wrappers$(OBJ) $(PCRELIB)pcre$(PCRE_SUFFIX) $(FCFLAGS)

example$(OBJ): example.f90
	$(FC) -c example.f90 $(FCFLAGS)


#
# Library
#
fcre$(OBJ): ../../src/fcre/fcre.f90
	$(FC) -c ../../src/fcre/fcre.f90 $(FCFLAGS)

pcre_wrappers$(OBJ): ../../src/fcre/pcre_wrappers.c
	$(CC) -c ../../src/fcre/pcre_wrappers.c $(PCREINCLUDE) $(CFLAGS)

mod_regex$(OBJ): ../../src/fcre/mod_regex.f90 fcre$(OBJ)
	$(FC) -c ../../src/fcre/mod_regex.f90 $(FCFLAGS)

#
# Cleaning is limited to a Linux-like environment (on Windows: Cygwin or MinGW)
#
clean:
	rm *$(OBJ)
	rm *.mod
	test -e test_regex && rm test_regex
	test -e wordcount && rm wordcount
