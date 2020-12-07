#
# Makefile to compile Fortran programs from multiple source files easily
#
.SUFFIXES: .f90

# compiler definitions
F90      = gfortran
F90FLAGS = -O2 -Wall

# program definitions
SRC_Pi   = RandomNumbers_mod.f90 Pi_with_Monte_Carlo.f90
OBJECTS_Pi = $(SRC_Pi:.f90=.o)

RandomNumbers: $(OBJECTS_Numbers)
	$(F90) $(F90FLAGS) $(OBJECTS_Numbers) -o $@

Pi_with_Monte_Carlo: $(OBJECTS_Pi)
	$(F90) $(F90FLAGS) $(OBJECTS_Pi) -o $@

.f90:
	$(F90) $(F90FLAGS) $? -o $@

.f90.o:
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *~ *.mod
