! a program to create random numbers
!  gfortran -o RandomNumbers  RandomNumbers.f90 RandomNumbers_mod.f90

PROGRAM FindPeriod
    USE RandomNumbers_mod
    IMPLICIT NONE

    integer :: i

   call lcg_init_seed(4)
   call set_parameter(7, 4, 15)           ! Set new parameter when (a,c,m) is given otherwise use default values in module
   
   do i=1, 1000                            
       print*, i, lcg_random()
   end do

  
  END PROGRAM FindPeriod