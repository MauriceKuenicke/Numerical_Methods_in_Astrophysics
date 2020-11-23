#
# Makefile to compile Fortran programs from multiple source files easily
#
.SUFFIXES: .f90

# compiler definitions
F90      = gfortran
F90FLAGS = -O2 -Wall

# program definitions
SRC     = RandomNumbers_mod.f90 RandomNumbers.f90
OBJECTS = $(SRC:.f90=.o)

all: RandomNumbers

RandomNumbers: $(OBJECTS)
	$(F90) $(F90FLAGS) $(OBJECTS) -o $@

.f90:
	$(F90) $(F90FLAGS) $? -o $@

.f90.o:
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *~ *.mod