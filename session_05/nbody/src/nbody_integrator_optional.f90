!!+nbody_integrator
!!
!! program: program to integrate an nbody sytem (sofar it only reads and
!!          prints a data set
!!
!!-
PROGRAM nbody_integrator
  USE parameters
  USE nbody_particles
  USE nbody_io
  IMPLICIT NONE
  
  ! declare local variables
  integer       :: n_particles, i
  real(8)          :: t, dt, t_last, r2
  real(8)          :: time, time_step, time_limit, time_step_half
  real, parameter :: G = 1.
  TYPE(nbodies) :: system


  ! allocates memory and reads the particle information
  CALL load_bodies(system)

  n_particles = get_n(system)
  time = 0.
  time_step = 0.05
  time_limit = 365.25

  time_step_half = time_step*0.5d0
  do while (time <= time_limit)
    call leapfrog_step1(n_particles, system, time_step_half)
    time = time + 10


  end do


  ! free memory
  CALL delete_nbodies(system)

END PROGRAM nbody_integrator

subroutine leapfrog_step1(n_particles, system, time_step_half)
USE parameters
USE nbody_particles
USE nbody_io
implicit NONE

integer, intent(in)        :: n_particles
TYPE(nbodies), intent(inout) :: system
real(8), intent(in)        :: time_step_half

! Declare local variable
integer :: i
TYPE(particle)            :: p

do i=1, n_particles
  p = get_body(system, i)
  print*, get_x(p)

end do
end subroutine