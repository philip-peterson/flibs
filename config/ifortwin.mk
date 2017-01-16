# Options for Intel Fortran - Windows
#
SPACE	=	\

SEP	=	/

FC	=	ifort
FFLAGS_NORMAL	=	/c
FFLAGS_DEBUG	=	/c /debug
FFLAGS_OPTIMISE	=	/c /fast

LD	=	ifort
LDFLAGS_NORMAL	=	
LDFLAGS_DEBUG	=	/debug
LDFLAGS_OPTIMISE	=	/fast
LDOUTPUT	=	/exe:$@

LIB	=	lib

OBJEXT	=	.obj
EXEEXT	=	.exe
MODEXT	=	.mod

DELETE	=	del /q
