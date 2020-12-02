!!+ bug03.f90
!!
!!
!!     compile with: gfortran bug03.f90 -o bug03
!!
!!     usage:        ./RandomNormal 100000
!!                   ./bug03 < RandomNumberGenerator/data/random_normal_DATA.dat
!!
!!
!!     expected result:
!!                     >>  how many numbers?
!!mean =       9.93131528E-01 stddev =       9.99308245E-01 stddev2 =       9.99308252E-01
!!mean =       9.99313153E+00 stddev =       9.99308245E-01 stddev2 =       9.99308252E-01
!!mean =       9.99931315E+01 stddev =       9.99308245E-01 stddev2 =       9.99308252E-01
!!mean =       9.99993132E+02 stddev =       9.99308245E-01 stddev2 =       9.99308249E-01
!!mean =       9.99999313E+03 stddev =       9.99308245E-01 stddev2 =       9.99308347E-01
!!mean =       9.99999931E+04 stddev =       9.99308245E-01 stddev2 =       9.99524237E-01
!!mean =       9.99999993E+05 stddev =       9.99308245E-01 stddev2 =       1.00359964E+00
!!
!!      The exact value of the numbers can vary because the seed is different and based on the subroutine system clock.
!!
!!-
PROGRAM Statistics
  IMPLICIT NONE

  INTEGER                            :: i, n
  REAL(8), DIMENSION(:), ALLOCATABLE :: values, input
  REAL(8)                            :: mean, stddev, stddev2, offset

  ! read in the number of numbers
  PRINT*, 'how many numbers?'
  !READ*, n
  !Added that always 100000 numbers are expected
  n = 100000

  ! allocate memory
  ALLOCATE(input(n), values(n))

  ! read in values (loop not needed)
  READ*, input

  DO i=0,6

    offset = 10**i

    values = input + offset

    ! compute mean using intrinsic sum
    mean   = SUM(values)/REAL(n)

    ! compute sample standard deviation
    stddev = SQRT( SUM((values - mean)**2) / (REAL(N-1)) )

    ! mathematically identical way to compute the standard deviation
    stddev2= SQRT( REAL(N)/REAL(N-1)*(SUM(values**2)/REAL(N) - mean**2) )

    PRINT '(3(A,ES20.8))', "mean = ", mean, " stddev = ", stddev, &
&                                           " stddev2 = ", stddev2

  END DO

  ! free memory
  DEALLOCATE(values)

END PROGRAM Statistics
