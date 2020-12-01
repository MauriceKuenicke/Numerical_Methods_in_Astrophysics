!!+ CalcRoots.f90
!!   
!!   This program exports two data files containing N lines with three random numbers each.
!!   The data in one file is calculated using a Linear Congruential Generator (LCG) while the data in the other
!!   is calculated using an already implemented function (random_uniform())
!!   The seed value and LCG parameter can be set to different values if needed.
!!
!!   compile with: > make RandomNumbers
!! 
!!   usage:        > ./RandomNumbers N
!!
!!   expected result:      lcg_1                        lcg_2                     lcg_3                     rndu_1                   rndu_2                    rndu_3
!!                          ...                         ...                        ...                      ...                     ...                         ...
!!                    0.10728156566619873       0.12665426731109619       0.79439151287078857       0.88996416330337524       0.88996416330337524       0.86044752597808838     
!!                   0.62646073102951050       0.60924059152603149        1.7296886071562767E-002   5.3155422210693359E-004   5.3155422210693359E-004  0.41944229602813721     
!!                    0.62061625719070435       0.56802546977996826       0.82260674238204956       0.63993996381759644       0.63993996381759644       0.11842513084411621     
!!                    0.82341104745864868       0.53700560331344604       0.81133425235748291       0.16621327400207520       0.16621327400207520       0.82214647531509399     
!!                     3.4954875707626343E-002  0.90772110223770142       0.13173256814479828       0.63008433580398560       0.63008433580398560       0.31096661090850830
!!		
!!-
PROGRAM RandomNumbers
    USE RandomNumbers_mod
    IMPLICIT NONE

    ! local variables declaration
    integer :: i, n, GetN
    real(kind=8) :: lcg_1, lcg_2, lcg_3, rndu_1, rndu_2, rndu_3
    

    ! set n given by cmd-line argument
    n = GetN()
    
    call lcg_init_seed(100)        ! Set seed value. The seed value is generated from the SYSTEM_CLOCK subroutine if no input is given
    call set_parameter()           ! Set new parameter when (a,c,m) is given otherwise use default values in module
  

   OPEN(UNIT=21, FILE='data/LCG_DATA.dat')
   OPEN(UNIT=22, FILE='data/random_uniform_DATA.dat')
   do i=1, n
       lcg_1 = lcg_random()
       lcg_2 = lcg_random()
       lcg_3 = lcg_random()
       WRITE(21,*) lcg_1, lcg_2, lcg_3

       rndu_1 = random_uniform(0.0,1.0)
       rndu_2 = random_uniform(0.0,1.0)
       rndu_3 = random_uniform(0.0,1.0)
       WRITE(22,*) rndu_1, rndu_1, rndu_3

       print*, lcg_1, lcg_2, lcg_3, rndu_1, rndu_1, rndu_3
   end do
   print*, "Numbers successfully exported into data/"


  
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
