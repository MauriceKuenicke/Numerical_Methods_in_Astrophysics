! a program to create random numbers
!  gfortran -o RandomNumbers  RandomNumbers.f90 RandomNumbers_mod.f90
PROGRAM RandomNormal
    USE RandomNumbers_mod
    IMPLICIT NONE

    integer :: i, n, GetN


    

    ! set n given by cmd-line argument
    n = GetN()

    !seed = 1227
    OPEN(UNIT=23, FILE='data/random_normal_DATA.dat')


    DO i=1,n
        PRINT*, random_normal()
        WRITE(23,*) random_normal()
    END DO


  
  END PROGRAM RandomNormal



  FUNCTION GetN() RESULT (n)
    IMPLICIT NONE
    
    INTEGER              :: n
    CHARACTER(LEN=32)    :: arg
  
    ! get n from command-line arguments
    IF (iargc()==1) THEN
       CALL getarg(1,arg)
       READ(arg, *, ERR=99) n        ! read n from string arg 
       RETURN                        ! on success exit here
    ELSE
       ! no or too many cmd-line arguments, aborting
       PRINT*, 'RandomNumbers requires exactly one argument.'
       PRINT*, '$ ./RandomNumbers <n>'
       STOP
    END IF
  
  99 PRINT*, 'Argument must be INTEGER value!' 
    PRINT*, '$ ./RandomNumbers <n>'
    STOP
  
  END FUNCTION GetN
