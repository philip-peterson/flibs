# makefile --
#     Makefile for the test_report and test_logging programs
#
#     For Compaq Visual Fortran we can use:
#     df x.f90 /link /exe:x.exe
#
#     -ox.exe is supported by g95 and gfortran
#
# For Compaq Visual Fortran:
#FC	=	df
#FCOPTS	=	
#EXEEXT	=	.exe
#OBJEXT	=	.obj
#OUTPUT	=	/link /exe:
#RM	=	del
#
#
#FC	=	g95
FC	=	gfortran
FCOPTS	=	-c
FPPOPTS=-D_ASSERT_ENABLE -cpp
EXEEXT	=	.exe
OBJEXT	=	.o
MODEXT	=	.mod
OUTPUT	=	-o # Trick: add a trailing blank to the OUTPUT macro
RM	=	rm

all	:	test_m_logger$(EXEEXT) test_m_exception$(EXEEXT) test_m_throwable$(EXEEXT) test_m_assert$(EXEEXT) \
      test_m_multilog$(EXEEXT)

test_m_logger$(EXEEXT)	:	test_m_logger.f90 m_logger$(OBJEXT)
	$(FC) test_m_logger.f90 m_logger$(OBJEXT) $(OUTPUT)test_m_logger$(EXEEXT)

m_logger$(OBJEXT)	:	../../src/reporting/m_logger.f90
	$(FC) $(FCOPTS) ../../src/reporting/m_logger.f90

test_m_multilog$(EXEEXT)	:	test_m_multilog.f90 m_multilog$(OBJEXT)
	$(FC) test_m_multilog.f90 m_multilog$(OBJEXT) $(OUTPUT)test_m_multilog$(EXEEXT)

m_multilog$(OBJEXT)	:	../../src/reporting/m_multilog.f90
	$(FC) $(FCOPTS) ../../src/reporting/m_multilog.f90

test_m_exception$(EXEEXT)	:	test_m_exception.f90 m_exception$(OBJEXT)
	$(FC) test_m_exception.f90 m_exception$(OBJEXT) $(OUTPUT)test_m_exception$(EXEEXT)

m_exception$(OBJEXT)	:	../../src/reporting/m_exception.f90
	$(FC) $(FCOPTS) ../../src/reporting/m_exception.f90

test_m_assert$(EXEEXT)	:	test_m_assert.f90 m_assert$(OBJEXT)
	$(FC) test_m_assert.f90 m_assert$(OBJEXT) m_exception$(OBJEXT) $(OUTPUT)test_m_assert$(EXEEXT)

m_assert$(OBJEXT)	:	../../src/reporting/m_assert.f90 m_exception$(OBJEXT)
	$(FC) $(FCOPTS) ../../src/reporting/m_assert.f90 $(FPPOPTS)

test_m_throwable$(EXEEXT)	:	test_m_throwable.f90 m_throwable$(OBJEXT)
	$(FC) test_m_throwable.f90 m_throwable$(OBJEXT) $(OUTPUT)test_m_throwable$(EXEEXT)

m_throwable$(OBJEXT)	:	../../src/reporting/m_throwable.f90
	$(FC) $(FCOPTS) ../../src/reporting/m_throwable.f90

#
# Not yet enabled: requires generated include file
#
test_report$(EXEEXT)	:	test_report.f90 report$(OBJEXT)
	$(FC) test_report.f90 report$(OBJEXT) $(OUTPUT)test_report$(EXEEXT)

report$(OBJEXT)	:	../../src/reporting/report.f90
	$(FC) $(FCOPTS) ../../src/reporting/report.f90

clean:
	$(RM) test_m_logger$(EXEEXT) test_m_expection$(EXEEXT) test_m_assertion$(EXEEXT) test_m_throwable$(EXEEXT) test_report$(EXEEXT) test_m_multilog$(EXEEXT)
	$(RM) m_logger$(OBJEXT)      m_expection$(OBJEXT)      m_assertion$(OBJEXT)      m_throwable$(OBJEXT)      reporting$(OBJEXT)   m_multilog$(OBJEXT)
	$(RM) m_logger$(MODEXT)      m_expection$(MODEXT)      m_assertion$(MODEXT) 