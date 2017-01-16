# Makefile for "string manipulation" tests and examples
#
# $Id: makefile,v 1.6 2016/12/04 11:40:54 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_token$(EXEEXT) \
		test_tokenlist$(EXEEXT) \
		text_tst$(EXEEXT) \
		test_textstr_func$(EXEEXT) \
		test_m_vstring$(EXEEXT) \
		test_m_vstringformat$(EXEEXT) \
		test_m_vstringlist$(EXEEXT) \
		test_keyvars$(EXEEXT) \
		test_translation$(EXEEXT) \
		mktranslation$(EXEEXT) \
		test_flexoutput$(EXEEXT) \
		test_command_args$(EXEEXT) \

all:	$(PROGRAMS)

test_token$(OBJEXT):	test_token.f90 tokenize$(OBJEXT)
	$(FC) $(FFLAGS) test_token.f90

tokenize$(OBJEXT):	../../src/strings/tokenize.f90
	$(FC) $(FFLAGS) ../../src/strings/tokenize.f90

test_token$(EXEEXT):	test_token$(OBJEXT) tokenize$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_token$(OBJEXT) tokenize$(OBJEXT)


test_tokenlist$(OBJEXT):	test_tokenlist.f90 tokenlist$(OBJEXT) tokenize$(OBJEXT)
	$(FC) $(FFLAGS) test_tokenlist.f90

tokenlist$(OBJEXT):	../../src/strings/tokenlist.f90
	$(FC) $(FFLAGS) ../../src/strings/tokenlist.f90

test_tokenlist$(EXEEXT):	test_tokenlist$(OBJEXT) tokenlist$(OBJEXT) tokenize$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_tokenlist$(OBJEXT) tokenlist$(OBJEXT) tokenize$(OBJEXT)


textstr$(OBJEXT):	../../src/strings/textstr.f90
	$(FC) $(FFLAGS) ../../src/strings/textstr.f90

text_tst$(OBJEXT):	text_tst.f90 textstr$(OBJEXT)
	$(FC) $(FFLAGS) text_tst.f90

text_tst$(EXEEXT):	text_tst$(OBJEXT) textstr$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) text_tst$(OBJEXT) textstr$(OBJEXT)


textstr_func$(OBJEXT):	../../src/strings/textstr_func.f90
	$(FC) $(FFLAGS) ../../src/strings/textstr_func.f90

test_textstr_func$(OBJEXT):	test_textstr_func.f90 textstr_func$(OBJEXT)
	$(FC) $(FFLAGS) test_textstr_func.f90

test_textstr_func$(EXEEXT):	test_textstr_func$(OBJEXT) textstr_func$(OBJEXT) textstr$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_textstr_func$(OBJEXT) textstr_func$(OBJEXT) textstr$(OBJEXT)


m_vstring$(OBJEXT):	../../src/strings/m_vstring.f90
	$(FC) $(FFLAGS) -x f95-cpp-input -D_VSTRING_ALLOCATABLE ../../src/strings/m_vstring.f90

test_m_vstring$(OBJEXT):	test_m_vstring.f90 m_vstring$(OBJEXT)
	$(FC) $(FFLAGS) -I../support -x f95-cpp-input test_m_vstring.f90

test_m_vstring$(EXEEXT):	test_m_vstring$(OBJEXT) m_vstring$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_vstring$(OBJEXT) m_vstring$(OBJEXT)


m_vstringformat$(OBJEXT):	../../src/strings/m_vstringformat.f90
	$(FC) $(FFLAGS) -x f95-cpp-input -D_VSTRING_ALLOCATABLE ../../src/strings/m_vstringformat.f90

test_m_vstringformat$(OBJEXT):	test_m_vstringformat.f90 m_vstringformat$(OBJEXT)
	$(FC) $(FFLAGS) -I../support -x f95-cpp-input test_m_vstringformat.f90

test_m_vstringformat$(EXEEXT):	test_m_vstringformat$(OBJEXT) m_vstringformat$(OBJEXT) m_vstring$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_vstringformat$(OBJEXT) m_vstringformat$(OBJEXT) m_vstring$(OBJEXT)


m_vstringlist$(OBJEXT):	../../src/strings/m_vstringlist.f90 ../../src/datastructures/qsortarray_template.f90
	$(FC) $(FFLAGS) -I../../src/datastructures -x f95-cpp-input -D_VSTRING_ALLOCATABLE -D_VSTRINGLIST_ALLOCATABLE ../../src/strings/m_vstringlist.f90

test_m_vstringlist$(OBJEXT):	test_m_vstringlist.f90 m_vstringlist$(OBJEXT)
	$(FC) $(FFLAGS) -I../support -x f95-cpp-input test_m_vstringlist.f90

test_m_vstringlist$(EXEEXT):	test_m_vstringlist$(OBJEXT) m_vstringlist$(OBJEXT) m_vstring$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_vstringlist$(OBJEXT) m_vstringlist$(OBJEXT) m_vstring$(OBJEXT)


keyvars$(OBJEXT):	../../src/strings/keyvars.f90
	$(FC) $(FFLAGS) ../../src/strings/keyvars.f90

test_keyvars$(OBJEXT):	test_keyvars.f90 keyvars$(OBJEXT)
	$(FC) $(FFLAGS) test_keyvars.f90

test_keyvars$(EXEEXT):	test_keyvars$(OBJEXT) keyvars$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_keyvars$(OBJEXT) keyvars$(OBJEXT)


mktranslation$(OBJEXT):	../../src/strings/mktranslation.f90
	$(FC) $(FFLAGS) ../../src/strings/mktranslation.f90

mktranslation$(EXEEXT):	mktranslation$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) mktranslation$(OBJEXT)

translation$(OBJEXT):	../../src/strings/translation.f90 translation.inc
	$(FC) $(FFLAGS) -I. ../../src/strings/translation.f90

translation.inc:	translations.inp mktranslation$(EXEEXT)
	./mktranslation$(EXEEXT) translations.inp

test_translation$(OBJEXT):	test_translation.f90 translation$(OBJEXT)
	$(FC) $(FFLAGS) test_translation.f90

test_translation$(EXEEXT):	test_translation$(OBJEXT) translation$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_translation$(OBJEXT) translation$(OBJEXT)

flexoutput$(OBJEXT):	../../src/strings/flexoutput.f90
	$(FC) $(FFLAGS) ../../src/strings/flexoutput.f90

test_flexoutput$(OBJEXT):	test_flexoutput.f90 flexoutput$(OBJEXT)
	$(FC) $(FFLAGS) test_flexoutput.f90

test_flexoutput$(EXEEXT):	test_flexoutput$(OBJEXT) flexoutput$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_flexoutput$(OBJEXT) flexoutput$(OBJEXT)

command_args$(OBJEXT):	../../src/strings/command_args.f90
	$(FC) $(FFLAGS) ../../src/strings/command_args.f90

test_command_args$(OBJEXT):	test_command_args.f90 command_args$(OBJEXT)
	$(FC) $(FFLAGS) test_command_args.f90

test_command_args$(EXEEXT):	test_command_args$(OBJEXT) command_args$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_command_args$(OBJEXT) command_args$(OBJEXT)



clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

