# Makefile for "computing" tests and examples
#
# $Id: makefile,v 1.11 2013/12/30 07:16:26 arjenmarkus Exp $
#
include ../../config/config.mk
include ../../config/options.mk

PROGRAMS	=	test_diff$(EXEEXT) \
		test_decimal$(EXEEXT) \
		test_m_coordinates$(EXEEXT) \
		test_libdate$(EXEEXT) \
		test_intervals$(EXEEXT) \
		test_annealing$(EXEEXT) \
		test_genalg$(EXEEXT) \
		test_pointsets$(EXEEXT) \
		test_unsigned$(EXEEXT) \
		test_f0noIEEE$(EXEEXT) \
		test_f0withIEEE$(EXEEXT) \
		test_random_permutation$(EXEEXT) \
		test_enum_space$(EXEEXT) \
		test_traverse_grid$(EXEEXT) \
		test_bobyqa$(EXEEXT) \
		test_newuoa$(EXEEXT) \

all:	$(PROGRAMS)

test_diff$(OBJEXT):	test_diff.f90 automdiff$(OBJEXT) select_precision$(OBJEXT)
	$(FC) $(FFLAGS) test_diff.f90

automdiff$(OBJEXT):	../../src/computing/automdiff.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/automdiff.f90

select_precision$(OBJEXT):	../../src/computing/select_precision.f90
	$(FC) $(FFLAGS) ../../src/computing/select_precision.f90

test_diff$(EXEEXT):	test_diff$(OBJEXT) automdiff$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_diff$(OBJEXT) automdiff$(OBJEXT) select_precision$(OBJEXT)


test_decimal$(OBJEXT):	test_decimal.f90 decimal$(OBJEXT)
	$(FC) $(FFLAGS) test_decimal.f90

decimal$(OBJEXT):	../../src/computing/decimal.f90
	$(FC) $(FFLAGS) ../../src/computing/decimal.f90

test_decimal$(EXEEXT):	test_decimal$(OBJEXT) decimal$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_decimal$(OBJEXT) decimal$(OBJEXT)


test_m_coordinates$(OBJEXT):	test_m_coordinates.f90 m_coordinates$(OBJEXT)
	$(FC) $(FFLAGS) test_m_coordinates.f90

m_coordinates$(OBJEXT):	../../src/computing/m_coordinates.f90
	$(FC) $(FFLAGS) ../../src/computing/m_coordinates.f90

test_m_coordinates$(EXEEXT):	test_m_coordinates$(OBJEXT) m_coordinates$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_m_coordinates$(OBJEXT) m_coordinates$(OBJEXT)


libdate$(OBJEXT):	../../src/computing/libdate.f90
	$(FC) $(FFLAGS) ../../src/computing/libdate.f90

test_libdate$(OBJEXT):	test_libdate.f90 libdate$(OBJEXT)
	$(FC) $(FFLAGS) test_libdate.f90

test_libdate$(EXEEXT):	test_libdate$(OBJEXT) libdate$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_libdate$(OBJEXT) libdate$(OBJEXT)


interval_simple$(OBJEXT):	../../src/computing/interval_simple.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/interval_simple.f90

test_intervals$(OBJEXT):	test_intervals.f90 interval_simple$(OBJEXT)
	$(FC) $(FFLAGS) test_intervals.f90

test_intervals$(EXEEXT):	test_intervals$(OBJEXT) interval_simple$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_intervals$(OBJEXT) interval_simple$(OBJEXT)


annealing$(OBJEXT):	../../src/computing/annealing.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/annealing.f90

test_annealing$(OBJEXT):	test_annealing.f90 annealing$(OBJEXT)
	$(FC) $(FFLAGS) test_annealing.f90

test_annealing$(EXEEXT):	test_annealing$(OBJEXT) annealing$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_annealing$(OBJEXT) annealing$(OBJEXT)


genetic_algorithms$(OBJEXT):	../../src/computing/genetic_algorithms.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/genetic_algorithms.f90

test_genalg$(OBJEXT):	test_genalg.f90 genetic_algorithms$(OBJEXT)
	$(FC) $(FFLAGS) test_genalg.f90

test_genalg$(EXEEXT):	test_genalg$(OBJEXT) genetic_algorithms$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_genalg$(OBJEXT) genetic_algorithms$(OBJEXT)


pointsets$(OBJEXT):	../../src/computing/pointsets.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/pointsets.f90

test_pointsets$(OBJEXT):	test_pointsets.f90 pointsets$(OBJEXT)
	$(FC) $(FFLAGS) test_pointsets.f90

test_pointsets$(EXEEXT):	test_pointsets$(OBJEXT) pointsets$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_pointsets$(OBJEXT) pointsets$(OBJEXT)


#
# Note: the program to generate the test input file is not dealt with
# - it would require a C compiler and therefore the test input file is
# stored in the repository
#
unsigned_integers$(OBJEXT):	../../src/computing/unsigned_integers.f90
	$(FC) $(FFLAGS) ../../src/computing/unsigned_integers.f90

test_unsigned$(OBJEXT):	test_unsigned.f90 unsigned_integers$(OBJEXT)
	$(FC) $(FFLAGS) test_unsigned.f90

test_unsigned$(EXEEXT):	test_unsigned$(OBJEXT) unsigned_integers$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_unsigned$(OBJEXT) unsigned_integers$(OBJEXT)

#
# Note: the leadzero module comes in two versions. The names are identical
# but one expects compiler support for IEEE NaN and Inf, the other does not.
#
f0noIEEE$(OBJEXT):	../../src/computing/f0noIEEE.f90
	$(FC) $(FFLAGS) ../../src/computing/f0noIEEE.f90

f0withIEEE$(OBJEXT):	../../src/computing/f0withIEEE.f90
	$(FC) $(FFLAGS) ../../src/computing/f0withIEEE.f90

test_f0noIEEE$(OBJEXT):	test_f0noIEEE.f90 f0noIEEE$(OBJEXT)
	$(FC) $(FFLAGS) test_f0noIEEE.f90

test_f0withIEEE$(OBJEXT):	test_f0withIEEE.f90 f0withIEEE$(OBJEXT)
	$(FC) $(FFLAGS) test_f0withIEEE.f90

test_f0noIEEE$(EXEEXT):	test_f0noIEEE$(OBJEXT) f0noIEEE$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_f0noIEEE$(OBJEXT) f0noIEEE$(OBJEXT)

test_f0withIEEE$(EXEEXT):	test_f0withIEEE$(OBJEXT) f0withIEEE$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_f0withIEEE$(OBJEXT) f0withIEEE$(OBJEXT)


random_permutation$(OBJEXT):	../../src/computing/random_permutation.f90
	$(FC) $(FFLAGS) ../../src/computing/random_permutation.f90

test_random_permutation$(OBJEXT):	test_random_permutation.f90 random_permutation$(OBJEXT)
	$(FC) $(FFLAGS) test_random_permutation.f90

test_random_permutation$(EXEEXT):	test_random_permutation$(OBJEXT) random_permutation$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_random_permutation$(OBJEXT) random_permutation$(OBJEXT)


enum_space$(OBJEXT):	../../src/computing/enum_space.f90
	$(FC) $(FFLAGS) ../../src/computing/enum_space.f90

test_enum_space$(OBJEXT):	test_enum_space.f90 enum_space$(OBJEXT)
	$(FC) $(FFLAGS) test_enum_space.f90

test_enum_space$(EXEEXT):	test_enum_space$(OBJEXT) enum_space$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_enum_space$(OBJEXT) enum_space$(OBJEXT)


traverse_grid$(OBJEXT):	../../src/computing/traverse_grid.f90 select_precision$(OBJEXT)
	$(FC) $(FFLAGS) ../../src/computing/traverse_grid.f90

test_traverse_grid$(OBJEXT):	test_traverse_grid.f90 traverse_grid$(OBJEXT) select_precision$(OBJEXT)
	$(FC) $(FFLAGS) test_traverse_grid.f90

test_traverse_grid$(EXEEXT):	test_traverse_grid$(OBJEXT) traverse_grid$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_traverse_grid$(OBJEXT) traverse_grid$(OBJEXT)


bobyqa$(OBJEXT):	../../src/computing/bobyqa.f
	$(FC) $(FFLAGS) ../../src/computing/bobyqa.f

test_bobyqa$(OBJEXT):	test_bobyqa.f90 bobyqa$(OBJEXT)
	$(FC) $(FFLAGS) test_bobyqa.f90

test_bobyqa$(EXEEXT):	test_bobyqa$(OBJEXT) bobyqa$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_bobyqa$(OBJEXT) bobyqa$(OBJEXT)


newuoa$(OBJEXT):	../../src/computing/newuoa.f
	$(FC) $(FFLAGS) ../../src/computing/newuoa.f

test_newuoa$(OBJEXT):	test_newuoa.f90 newuoa$(OBJEXT)
	$(FC) $(FFLAGS) test_newuoa.f90

test_newuoa$(EXEEXT):	test_newuoa$(OBJEXT) newuoa$(OBJEXT)
	$(LD) $(LDFLAGS) $(LDOUTPUT) test_newuoa$(OBJEXT) newuoa$(OBJEXT)


clean:	
	$(DELETE) *$(OBJEXT)
	$(DELETE) $(PROGRAMS)
	$(DELETE) *$(MODEXT)

