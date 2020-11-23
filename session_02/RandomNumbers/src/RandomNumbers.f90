! a program to create random numbers
PROGRAM RandomNumbers
  USE RandomNumbers_mod
  IMPLICIT NONE

  ! declaration of variables and functions
  INTEGER :: i, n, seed, GetN

  ! set n given by cmd-line argument
  n = GetN()

  ! initialize random numbers
  seed = init_random_seed(2337)

  ! write n and n random numbers to STDOUT
  PRINT*, n
  DO i=1,n
     
     PRINT*, random_uniform(1.0,100.0)

  END DO

END PROGRAM RandomNumbers


! function that returns an integer value for n given
! as a command-line argument, if no argument was given
! or more than one, or if the argument was not an integer
! it returns an error message
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
