! a module with different functions to create random numbers
! it is based on the intrinsic uniform PRN-generator
MODULE RandomNumbers_mod
  IMPLICIT NONE
  PRIVATE
  
  ! Double precision byte size
  INTEGER, PARAMETER            :: dp = kind(0.d0)

  INTEGER, PARAMETER            :: LONG = SELECTED_INT_KIND(15)
  INTEGER(KIND=LONG)            :: a
  INTEGER(KIND=LONG)            :: c 
  INTEGER(KIND=LONG)            :: m 
  INTEGER(KIND=LONG)            :: seed

  ! interface for random_uniform, which can either be called with or 
  ! without a range
  INTERFACE random_uniform
     MODULE PROCEDURE random_uniform_norange, random_uniform_zeroto,&
    &                 random_uniform_range
  END INTERFACE

  ! interface for init_random_seed, which can be called with or without
  ! a seed integer, in the latter case the seed is taken from the
  ! system clock
  INTERFACE init_random_seed
     MODULE PROCEDURE init_random_noseed, init_random_withseed
  END INTERFACE
 
  ! interface for lcg_init_seed, which can either be called with or 
  ! without a seed
  INTERFACE lcg_init_seed
     MODULE PROCEDURE lcg_init_noseed, lcg_init_withseed
  END INTERFACE
  
  ! interface for set_parameter, which can either be called with or 
  ! without a seed
  INTERFACE set_parameter
     MODULE PROCEDURE change_parameter, use_default_parameter
  END INTERFACE

  ! make generic functions public
  PUBLIC :: random_normal
  PUBLIC :: random_uniform
  PUBLIC :: init_random_seed
  PUBLIC :: lcg_random
  PUBLIC :: lcg_init_seed
  PUBLIC :: set_parameter

CONTAINS

  ! function to initialize the intrinsic PRN-generator
  !
  FUNCTION init_random_noseed() RESULT (base_seed)
    INTEGER                            :: i, n, base_seed
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
     
    i = 0 ! initialize i to avoid spurious warning of compiler
     
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))
          
    CALL SYSTEM_CLOCK(COUNT=base_seed)
          
    seed = base_seed + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
          
    DEALLOCATE(seed)
  END FUNCTION init_random_noseed

  ! function to initialize the intrinsic PRN-generator
  !
  FUNCTION init_random_withseed(in_seed) RESULT (base_seed)
    INTEGER, INTENT(in)                :: in_seed
    INTEGER                            :: i, n, base_seed
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
          
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))

    i = 0 ! initialize i to avoid spurious warning of compiler
          
    base_seed = in_seed          
    seed      = base_seed + 37 * (/ (i - 1, i = 1, n) /)

    CALL RANDOM_SEED(PUT = seed)
          
    DEALLOCATE(seed)
  END FUNCTION init_random_withseed


  ! function that returns a single uniformly distributed random number
  ! in the standard range 0 <= r < 1
  FUNCTION random_uniform_norange() RESULT (r)
    IMPLICIT NONE
    REAL :: r

    CALL RANDOM_NUMBER(r)       ! use the intrinsic PRN-generator
    
  END FUNCTION random_uniform_norange

  ! function that returns a single uniformly distributed random number
  ! in the range between 0 <= r < u
  FUNCTION random_uniform_zeroto(u) RESULT (r)
    IMPLICIT NONE
    REAL, INTENT(in) :: u       ! upper limit for range
    REAL             :: r

    CALL RANDOM_NUMBER(r)       ! use the intrinsic PRN-generator
    r = r*u                     ! correct for desired upper limit

  END FUNCTION random_uniform_zeroto

  ! function that returns a single uniformly distributed random number
  ! in the range l <= r < u
  FUNCTION random_uniform_range(l,u) RESULT (r)
    IMPLICIT NONE
    REAL, INTENT(in) :: l, u    ! lower and upper limit of range
    REAL             :: r

    CALL RANDOM_NUMBER(r)       ! use the intrinsic PRN-generator
    r = l + r*(u-l)             ! correct for desired range

  END FUNCTION random_uniform_range


  !-----------------------------------------------------------------------
  !Generate random number with LCG
  FUNCTION lcg_random() RESULT (r)
    IMPLICIT NONE
    REAL                   :: r

    seed = MOD(a*seed+c, m)
    r = REAL(seed)/REAL(m)

  END FUNCTION lcg_random

  ! Routine that changes the seed value
  subroutine lcg_init_withseed(n)
    implicit NONE
    integer :: n
    seed = n
  end subroutine lcg_init_withseed

  ! Routine that creates a seed value from the system clock
  subroutine lcg_init_noseed()
    implicit NONE

    CALL SYSTEM_CLOCK(COUNT=seed)     ! using intrinsic SYSTEM_CLOCK function

  end subroutine lcg_init_noseed

  ! Routine that changes the lcg parameter to the given input values
  subroutine change_parameter(a_new, c_new, m_new)
    implicit none
    integer :: a_new, c_new, m_new
    a = a_new
    c = c_new
    m = m_new
    print*, "Parameters changed for LCG to: ", "a=",a, "c=",c, "m=",m
  end subroutine change_parameter
  
  ! Alternative Routine which sets the default parameter
  subroutine use_default_parameter()
    implicit none
    
    a = 65539
    c = 0
    m = 2_LONG**31
    print*, "Use default parameter for LCG: ", "a=",a, "c=",c, "m=",m

  end subroutine use_default_parameter

  ! Function that generates normally distributed random numbers using the polar method.
  FUNCTION random_normal() RESULT(touple_A)
    IMPLICIT NONE
    REAL(dp) :: touple_A, touple_B
    REAL(dp) :: random_A, random_B, q

    q = 2.0

    DO WHILE(q>=1 .OR. q<=0)
       random_A = random_uniform(-1.0,1.0)
       random_B = random_uniform(-1.0,1.0)
       q = random_A*random_A + random_B*random_B
    END DO

    touple_A = sqrt(-2*log(q)/q)*random_A
    touple_B = sqrt(-2*log(q)/q)*random_B
  END FUNCTION random_normal
 
END MODULE RandomNumbers_mod
