#
# Makefile to compile Fortran programs from multiple source files easily
#
.SUFFIXES: .f90

# compiler definitions
F90      = gfortran
F90FLAGS = -O2 -Wall

SRC_rndm     = RandomNumbers_mod.f90 RandomNumbers.f90
OBJECTS_rndm = $(SRC_rndm:.f90=.o)

SRC_period     = RandomNumbers_mod.f90 FindPeriod.f90
OBJECTS_period = $(SRC_period:.f90=.o)

SRC_normal   = RandomNumbers_mod.f90 RandomNormal.f90
OBJECTS_normal = $(SRC_normal:.f90=.o)

RandomNormal: $(OBJECTS_normal)
	$(F90) $(F90FLAGS) $(OBJECTS_normal) -o $@

RandomNumbers: $(OBJECTS_rndm)
	$(F90) $(F90FLAGS) $(OBJECTS_rndm) -o $@

FindPeriod: $(OBJECTS_period)
	$(F90) $(F90FLAGS) $(OBJECTS_period) -o $@

.f90:
	$(F90) $(F90FLAGS) $? -o $@

.f90.o:
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *~ *.mod *.dat