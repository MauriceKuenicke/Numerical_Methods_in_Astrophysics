! a program to create random numbers
PROGRAM RandomNumbers
  USE RandomNumbers_mod
  IMPLICIT NONE

  ! declaration of variables and functions
  INTEGER        :: i, n, GetN, k, seed
  TYPE :: VECTOR
     REAL(KIND=8) :: x_coord, y_coord, z_coord
  END TYPE VECTOR

  TYPE(VECTOR) :: vec
  
  ! set n given by cmd-line argument
  n = GetN()

  ! initialize random numbers
  seed = init_random_seed(2337)
  
  
  ! write n and n random numbers to STDOUT
  PRINT*, n
  outer: DO i=1,n
    inner: DO k=1,3
       IF(k==1) THEN
          vec%x_coord = lcg_random()

          !PRINT*, vec%x_coord
       END IF
       IF(k==2) THEN
          vec%y_coord = lcg_random()

          !PRINT*, vec%y_coord
       END IF
       IF(k==3) THEN
          vec%z_coord = lcg_random()

          !PRINT*, vec%z_coord
       END IF

       !OPEN(UNIT=10, FILE='LCG_DATA.dat')
       !WRITE(10,*) lcg_random()
    END DO inner

    PRINT*, vec
    OPEN(UNIT=10, FILE='LCG_DATA.dat')
    WRITE(10,*) vec

  END DO outer

  outer2: DO i=1,n
    inner2: DO k=1,3
       IF(k==1) THEN
          vec%x_coord = random_uniform(0.0,1.0)

          !PRINT*, vec%x_coord
       END IF
       IF(k==2) THEN
          vec%y_coord = random_uniform(0.0,1.0)

          !PRINT*, vec%y_coord
       END IF
       IF(k==3) THEN
          vec%z_coord = random_uniform(0.0,1.0)

          !PRINT*, vec%z_coord
       END IF

       !OPEN(UNIT=10, FILE='LCG_DATA.dat')
       !WRITE(10,*) lcg_random()
    END DO inner2

    PRINT*, vec
    OPEN(UNIT=20, FILE='random_uniform_DATA.dat')
    WRITE(20,*) vec

  END DO outer2
  
  OPEN(UNIT=21, FILE='random_uniform_DATA_test.dat')
  do i=1, n
     WRITE(21,*) lcg_random(), lcg_random(), lcg_random()
  end do 

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
