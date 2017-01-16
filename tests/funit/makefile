# makefile --
#     Build the test_ftnunit program
#
F95 = gfortran
FCOPTS = -c -I../../src/funit
EXEEXT = .exe
OBJEXT = .o

all:	test_ftnunit$(EXEEXT) test_tridiag$(EXEEXT) test_store$(EXEEXT)

test_ftnunit$(EXEEXT):	ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT) test_ftnunit$(OBJEXT)
	$(F95) -o test_ftnunit$(EXEEXT) test_ftnunit$(OBJEXT) ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)

test_tridiag$(EXEEXT):	ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT) test_tridiag$(OBJEXT)
	$(F95) -o test_tridiag$(EXEEXT) test_tridiag$(OBJEXT) ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)

test_store$(EXEEXT):	test_store$(OBJEXT) ftnunit_store$(OBJEXT) ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)
	$(F95) -o test_store$(EXEEXT) test_store$(OBJEXT) ftnunit_store$(OBJEXT) ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)

test_ftnunit$(OBJEXT):	test_ftnunit.f90 ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)
	$(F95) $(FCOPTS) test_ftnunit.f90

test_tridiag$(OBJEXT):	test_tridiag.f90 ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)
	$(F95) $(FCOPTS) test_tridiag.f90

test_store$(OBJEXT):	test_store.f90 ftnunit_store$(OBJEXT) ftnunit$(OBJEXT) ftnunit_hooks$(OBJEXT)
	$(F95) $(FCOPTS) test_store.f90

ftnunit$(OBJEXT):	../../src/funit/ftnunit.f90 ftnunit_hooks$(OBJEXT)
	$(F95) $(FCOPTS) ../../src/funit/ftnunit.f90

ftnunit_hooks$(OBJEXT):	../../src/funit/ftnunit_hooks.f90
	$(F95) $(FCOPTS) ../../src/funit/ftnunit_hooks.f90

ftnunit_store$(OBJEXT):	../../src/funit/ftnunit_store.f90
	$(F95) $(FCOPTS) ../../src/funit/ftnunit_store.f90
