!!+ bug.f90
!!   
!!   This is a little program that prints the values of an exponential
!!   function for x-values spaced logarithmically. It will ask the 
!!   user for the number of points to print and the minimum and maximum
!!   values for x.
!!
!!   compile with: > gfortran bug.f90 -o bug
!! 
!!   usage:        > ./bug
!!                 Enter a number for n:        10
!!                 Enter a minimum value for x: 0.01
!!                 Enter a maximum value for x: 30.0
!!
!!   expected result:
!!		
!!                   0.01000000     0.99714693
!!                   0.02434147     0.99306942
!!                   0.05925072     0.98321371
!!                   0.14422497     0.95963034
!!                   0.35106476     0.90456219
!!                   0.85454340     0.78336600
!!                   2.08008415     0.55194341
!!                   5.06323038     0.23536042
!!                  12.32464650     0.02956017
!!                  30.00000000     0.00018944
!!
!!   However, a little bug has sneaked into the program, and you will
!!   need to fix it before you will get the correct result.
!!   
!!-
PROGRAM bug1
    IMPLICIT NONE
  
    ! local variables
    INTEGER      :: i, n
    REAL(KIND=8) :: x, xmin, xmax, xstep, fofx, exponent
  
    ! prompt user for a number of input values
    ! and read values from STDIN              
    PRINT*, "Enter a number for n:        "  ! the PRINT and READ statements
    READ*, n                                 ! automatically write to and
    PRINT*, "Enter a minimum value for x: "  ! read from STDOUT and STDIN
    READ*, xmin                              ! the * is a FORMAT-descriptor 
    PRINT*, "Enter a maximum value for x: "  ! in both cases
    READ*, xmax
  
  
    ! compute a logarithmic step size in order to get evenly spaced
    ! log(x)-values                                                
    xstep = log10(xmax/xmin)
  
    ! do-loop that computes and prints n x-values within [xmin,xmax]
    DO i=0, n-1
        
       !x    = xmin*10**(i/(n-1)*xstep) ! compute x for i-th step of the loop  
                                        ! i/(n-1) will give 0 for i in [0,9] because of integer division
                                        ! the divison will result some value < 1 which gets rounded to the integer 0
                                        ! fix using REAL(a, KIND=8)
        exponent = REAL(i, kind=8)/REAL(n-1, kind=8)*xstep
        x = xmin*10**exponent
        fofx = exp(-x/3.5)              ! compute f(x), in this case it
                                        ! could be the exponential profile
                                        ! of a disk galaxy
  
        PRINT"(2f15.8)", x, fofx         ! print x and fofx to STDOUT, note
                                        ! the format descriptor replacing *
    END DO
  
  END PROGRAM bug1