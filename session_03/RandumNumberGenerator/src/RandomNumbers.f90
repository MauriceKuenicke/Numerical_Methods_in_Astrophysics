! a program to create random numbers
PROGRAM RandomNumbers
    USE RandomNumbers_mod
    IMPLICIT NONE

    integer :: i, n, GetN

    ! set n given by cmd-line argument
    n = GetN()
    

    OPEN(UNIT=21, FILE='LCG_DATA.dat')
    OPEN(UNIT=22, FILE='random_uniform_DATA.dat')
    do i=1, n
       WRITE(21,*) lcg_random(), lcg_random(), lcg_random()
       WRITE(22,*) random_uniform(0.0,1.0), random_uniform(0.0,1.0), random_uniform(0.0,1.0)
    end do
    
  
  END PROGRAM RandomNumbers



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
