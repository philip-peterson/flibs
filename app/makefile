# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.1 2009/02/02 05:25:03 arjenmarkus Exp $
#
include ../config/config.mk
include ../config/options.mk

PROGRAMS	=	testcov$(EXEEXT) \

all:	$(PROGRAMS)

testcov$(OBJEXT):	testcov.f90 testcov_reg.$(OBJEXT)
	$(FC) $(FFLAGS) testcov.f90

testcov_reg$(OBJEXT):	testcov_reg.f90
	$(FC) $(FFLAGS) testcov_reg.f90

testcov$(EXEEXT):	testcov$(OBJEXT) testcov_reg$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) testcov$(OBJEXT) testcov_reg$(OBJEXT)

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

