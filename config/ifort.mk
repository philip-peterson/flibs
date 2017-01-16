# Options for Intel Fortran - Linux
#
SPACE	=	\

SEP	=	/

FC	=	ifort
FFLAGS_NORMAL	=	-c
FFLAGS_DEBUG	=	-c -g
FFLAGS_OPTIMISE	=	-c -fast

LD	=	ifort
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	-g
LDFLAGS_OPTIMISE	=	
LDOUTPUT	=	-o $@

LIB	=	ar r

OBJEXT	=	.o
EXEEXT	=	
MODEXT	=	.mod

DELETE	=	rm -f
