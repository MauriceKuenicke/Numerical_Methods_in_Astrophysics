#
# Makefile to compile Fortran programs from multiple source files easily
#
.SUFFIXES: .f90

# compiler definitions
F90      = gfortran
F90FLAGS = -O2 -Wall

# program definitions
SRC_num  = numericalIntegration_mod.f90 myfuncs.f90 test_integration.f90
OBJECTS_num = $(SRC_num:.f90=.o)

all: test_integration

test_integration: $(OBJECTS_num)
	$(F90) $(F90FLAGS) $(OBJECTS_num) -o $@

.f90:
	$(F90) $(F90FLAGS) $? -o $@

.f90.o:
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *~ *.mod *.png