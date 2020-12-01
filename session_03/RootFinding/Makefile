#
# Makefile to compile Fortran programs from multiple source files easily
#
.SUFFIXES: .f90

# compiler definitions
F90      = gfortran
F90FLAGS = -O2 -Wall

SRC_roots     = MyFuncs.f90 RootFinding.f90 CalcRoots.f90
OBJECTS_roots = $(SRC_roots:.f90=.o)

SRC_complex     = MyFuncs.f90 RootFinding.f90 CalcRoots_Complex.f90
OBJECTS_complex = $(SRC_complex:.f90=.o)

CalcRoots: $(OBJECTS_roots)
	$(F90) $(F90FLAGS) $(OBJECTS_roots) -o $@

CalcRootsComplex: $(OBJECTS_complex)
	$(F90) $(F90FLAGS) $(OBJECTS_complex) -o $@

.f90:
	$(F90) $(F90FLAGS) $? -o $@

.f90.o:
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *~ *.mod *.dat