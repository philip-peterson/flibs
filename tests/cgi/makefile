# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.2 2013/01/21 08:12:38 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_cgi$(EXEEXT) \
	test_fcgi$(EXEEXT) \
	guess_number_fcgi$(EXEEXT) \
	run_long_computation$(EXEEXT) \
	long_computation$(EXEEXT) \

all:	$(PROGRAMS)

test_cgi$(OBJEXT):	test_cgi.f90 cgi_protocol$(OBJEXT)
	$(FC) $(FFLAGS) test_cgi.f90

test_fcgi$(OBJEXT):	test_fcgi.f90 fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT)
	$(FC) $(FFLAGS) test_fcgi.f90

guess_number_fcgi$(OBJEXT):	guess_number_fcgi.f90 fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT)
	$(FC) $(FFLAGS) guess_number_fcgi.f90

run_long_computation$(OBJEXT):	run_long_computation.f90 fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT)
	$(FC) $(FFLAGS) run_long_computation.f90

cgi_protocol$(OBJEXT):	../../src/cgi/cgi_protocol.f90
	$(FC) $(FFLAGS) ../../src/cgi/cgi_protocol.f90 -I../../src/cgi -I../../src/datastructures

fcgi_protocol$(OBJEXT):	../../src/cgi/fcgi_protocol.f90
	$(FC) $(FFLAGS) ../../src/cgi/fcgi_protocol.f90 -I../../src/cgi -I../../src/datastructures

test_cgi$(EXEEXT):	test_cgi$(OBJEXT) cgi_protocol$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_cgi$(OBJEXT) cgi_protocol$(OBJEXT)

test_fcgi$(EXEEXT):	test_fcgi$(OBJEXT) fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_fcgi$(OBJEXT) fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT) -L. -lfcgi

guess_number_fcgi$(EXEEXT):	guess_number_fcgi$(OBJEXT) fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) guess_number_fcgi$(OBJEXT) fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT) -L. -lfcgi

run_long_computation$(EXEEXT):	run_long_computation$(OBJEXT) fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) run_long_computation$(OBJEXT) fcgi_protocol$(OBJEXT) cgi_protocol$(OBJEXT) -L. -lfcgi

long_computation$(EXEEXT):	long_computation.f90
	$(LD) $(LDFLAGS) $(LDOUTPUT) long_computation.f90

clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

