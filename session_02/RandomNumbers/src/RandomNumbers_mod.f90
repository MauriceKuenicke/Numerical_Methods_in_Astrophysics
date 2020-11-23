! a module with different functions to create random numbers
! it is based on the intrinsic uniform PRN-generator
MODULE RandomNumbers_mod
  IMPLICIT NONE
  PRIVATE

  ! interface for random_uniform, which can be either be called with or 
  ! without a range
  INTERFACE random_uniform
     MODULE PROCEDURE random_uniform_norange, random_uniform_zeroto,&
    &                 random_uniform_range
  END INTERFACE

  ! interface for init_random_seed, which can be call with or without
  ! a seed integer, in the latter case the seed is taken from the
  ! system clock
  INTERFACE init_random_seed
     MODULE PROCEDURE init_random_noseed, init_random_withseed
  END INTERFACE

  ! make generic functions public
  PUBLIC :: random_uniform
  PUBLIC :: init_random_seed

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

END MODULE RandomNumbers_mod
