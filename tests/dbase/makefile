# Makefile for "dbase" test
#
# $Id: makefile,v 1.1 2013/10/05 10:53:13 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_dbasefile$(EXEEXT) \

all:	$(PROGRAMS)

test_dbasefile$(OBJEXT):	test_dbasefile.f90 dbasefile$(OBJEXT)
	$(FC) $(FFLAGS) test_dbasefile.f90

dbasefile$(OBJEXT):	../../src/dbase/dbasefile.f90
	$(FC) $(FFLAGS) ../../src/dbase/dbasefile.f90

test_dbasefile$(EXEEXT):	test_dbasefile$(OBJEXT) dbasefile$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_dbasefile$(OBJEXT) dbasefile$(OBJEXT)


clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

