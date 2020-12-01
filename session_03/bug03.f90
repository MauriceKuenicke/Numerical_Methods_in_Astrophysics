! a short program that computes mean and standard deviation
PROGRAM Statistics
  IMPLICIT NONE

  INTEGER                            :: i, n
  REAL(8), DIMENSION(:), ALLOCATABLE :: values, input
  REAL(8)                            :: mean, stddev, stddev2, offset

  ! read in the number of numbers
  PRINT*, 'how many numbers?'
  !READ*, n
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
