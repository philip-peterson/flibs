# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.1 2012/11/04 12:57:07 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_readchar$(EXEEXT) \

all:	$(PROGRAMS)

test_readchar$(OBJEXT):	test_readchar.f90 readchar$(OBJEXT)
	$(FC) $(FFLAGS) test_readchar.f90

readchar$(OBJEXT):	../../src/streams/readchar.f90
	$(FC) $(FFLAGS) ../../src/streams/readchar.f90

test_readchar$(EXEEXT):	test_readchar$(OBJEXT) readchar$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_readchar$(OBJEXT) readchar$(OBJEXT)


clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

