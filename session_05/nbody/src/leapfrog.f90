MODULE leapfrog
    use parameters
    IMPLICIT NONE
    PRIVATE
  
    PUBLIC :: position_drift, velocity_kick, calculate_force_acceleration
  
  CONTAINS
  
  subroutine position_drift(n_particles,x,v,time_step_half)
    implicit NONE

    integer, intent(in)    :: n_particles
    real(DP), intent(out)   :: x(3, n_particles)
    real(DP), intent(in)    :: v(3, n_particles)
    real(DP), intent(in)    :: time_step_half

    integer :: i

    do i=1, n_particles
        x(1,i) = x(1,i) + time_step_half * v(1,i)
        x(2,i) = x(2,i) + time_step_half * v(2,i)
        x(3,i) = x(3,i) + time_step_half * v(3,i)
    end do

   end subroutine position_drift

subroutine velocity_kick(n_particles, v,a, time_step)
    implicit none

    integer, intent(in)   :: n_particles 
    real(DP), intent(out) :: v(3, n_particles)
    real(DP), intent(in)  :: a(3, n_particles)
    real(DP), intent(in)  :: time_step

    integer :: i

    do i=1, n_particles
        v(1,i) = v(1,i) + time_step * a(1,i)
        v(2,i) = v(2,i) + time_step * a(2,i)
        v(3,i) = v(3,i) + time_step * a(3,i)
    end do
end subroutine velocity_kick

subroutine calculate_force_acceleration(n_particles, m, x, a)
    implicit none
    integer, intent(in)   :: n_particles
    real(DP), intent(in)  :: x(3, n_particles)
    real(DP), intent(in)  :: m(n_particles)
    real(DP), intent(out) :: a(3, n_particles)

    integer :: i,j
    real(DP) :: dx,dy,dz,rr

    do i=1, n_particles
        a(1, i) = 0._DP
        a(2, i) = 0._DP
        a(3, i) = 0._DP

        do j = 1, n_particles
            if (i /= j) then
                dx = x(1,i) - x(1,j)
                dy = x(2,i) - x(2,j)
                dz = x(3,i) - x(3,j)
                rr = sqrt(dx*dx + dy*dy + dz*dz)

                a(1,i) = a(1,i) - G*m(j)/(rr*rr*rr) * dx
                a(2,i) = a(2,i) - G*m(j)/(rr*rr*rr) * dy
                a(3,i) = a(3,i) - G*m(j)/(rr*rr*rr) * dz
            end if
        end do
    end do
    return
end subroutine calculate_force_acceleration
  
  END MODULE leapfrog