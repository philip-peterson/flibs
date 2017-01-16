# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.1 2013/12/22 15:36:51 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_bmp$(EXEEXT) \

all:	$(PROGRAMS)

test_bmp$(OBJEXT):	bmpfile$(OBJEXT)
	$(FC) $(FFLAGS) test_bmp.f90

bmpfile$(OBJEXT):	../../src/images/bmpfile.f90 integral_types$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/images/bmpfile.f90

integral_types$(OBJEXT):	../../src/images/integral_types.f90
	$(FC) $(FFLAGS) ../../src/images/integral_types.f90

# Note: integral_types.f90 does not contain actual code, only definitions
test_bmp$(EXEEXT):	test_bmp$(OBJEXT) bmpfile$(OBJEXT) integral_types$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_bmp$(OBJEXT) bmpfile$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

