!!+nbody_io.f90
!!
!! module: provides functions to read and write nbody data, supported format
!!         is one line of header with number of particles N follow by N
!!         lines with particles data (mass, pos, vel --> 7 REALs)
!!       
!!-
MODULE nbody_io
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: load_bodies

CONTAINS

  !
  ! read particles from STDIN and load them into this also allocating
  ! the required memory
  !
  SUBROUTINE load_bodies(n_particles, x, v, a, m)
    use parameters
    IMPLICIT NONE

    integer                                               :: status, i
    integer, intent(out)                                  :: n_particles
    real(8), dimension (:,:), allocatable,  intent(out)   :: x, v, a
    real(8), dimension (:), allocatable, intent(out)      :: m
    

    ! Allocate initial particle conditions
    READ*, n_particles
    ALLOCATE(m(n_particles), &
             x(3, n_particles), & 
             v(3, n_particles), &
             a(3, n_particles), STAT=status)     
    IF(status/=0) STOP

    DO i=1, n_particles
        READ*, m(i), x(1,i), x(2,i), x(3,i), &
               v(1,i), v(2,i), v(3,i)
    END DO

  END SUBROUTINE load_bodies

END MODULE nbody_io
