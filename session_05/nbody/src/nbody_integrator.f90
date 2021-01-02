PROGRAM nbody_integrator
    USE parameters
    USE nbody_particles
    USE nbody_io
    IMPLICIT NONE

    ! declare local variables
    integer          :: n_particles, i, status
    real(8)          :: time, time_step, time_limit, time_step_half, E
    real(8), dimension (:,:), allocatable :: x, v, a 
    real(8), dimension (:), allocatable :: m


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

    time = 0.
    time_step = 0.0002
    time_limit = 10

    time_step_half = time_step*0.5
    open(unit=77, file='out.dat')
    do while(time <= time_limit)
        call leapfrog_update_positions(n_particles,x,v,time_step_half)
        time = time + time_step_half
        call calculate_force_acceleration(n_particles, m, x, a)
        call leapfrog_update_velocities(n_particles, x,v,a, time_step, time_step_half)
        time = time + time_step_half
        call calculate_system_energy(n_particles, x, v, m, E)
        write(77,*) time, x, E

    end do
    

end program


subroutine leapfrog_update_positions(n_particles,x,v,time_step_half)
    implicit NONE

    integer, intent(in) :: n_particles
    real(8), intent(out)   :: x(3, n_particles)
    real(8), intent(in)    :: v(3, n_particles)
    real(8), intent(in)    :: time_step_half

    integer :: i

    do i=1, n_particles
        x(1,i) = x(1,i) + time_step_half * v(1,i)
        x(2,i) = x(2,i) + time_step_half * v(2,i)
        x(3,i) = x(3,i) + time_step_half * v(3,i)
    end do

end subroutine leapfrog_update_positions

subroutine leapfrog_update_velocities(n_particles, x,v,a, time_step, time_step_half)
    implicit none

    integer, intent(in) :: n_particles 
    real(8), intent(out) :: x(3, n_particles), v(3, n_particles)
    real(8), intent(in) :: a(3, n_particles)
    real(8), intent(in)  :: time_step, time_step_half

    integer :: i

    do i=1, n_particles
        v(1,i) = v(1,i) + time_step * a(1,i)
        v(2,i) = v(2,i) + time_step * a(2,i)
        v(3,i) = v(3,i) + time_step * a(3,i)

        x(1,i) = x(1,i) + time_step_half * v(1,i)
        x(2,i) = x(2,i) + time_step_half * v(2,i)
        x(3,i) = x(3,i) + time_step_half * v(3,i)
    end do
end subroutine leapfrog_update_velocities


subroutine calculate_force_acceleration(n_particles, m, x, a)
    implicit none
    integer, intent(in)  :: n_particles
    real(8), intent(in)  :: x(3, n_particles)
    real(8), intent(in)  :: m(n_particles)
    real(8), intent(out) :: a(3, n_particles)
    real(8), parameter  :: G = 1.

    integer :: i,j
    real(8) :: dx,dy,dz,rr

    do i=1, n_particles
        a(1, i) = 0.0d0
        a(2, i) = 0.0d0
        a(3, i) = 0.0d0

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

subroutine calculate_system_energy(n_particles, x, v, m, E)
    implicit NONE
    integer, intent(in)  :: n_particles
    real(8), intent(in)  :: x(3, n_particles), v(3, n_particles), m(n_particles)
    real(8), intent(out) :: E

    ! Local variables 
    real(8)   :: K, U, dx, dy, dz, rr
    real(8), parameter  :: G = 1.
    integer   :: i, j

    ! Kinetic energy
    K = 0.d0
    do i=i, n_particles
        K = K + 1./2. * m(i) * v(1,i) * v(1,i)
        K = K + 1./2. * m(i) * v(2,i) * v(2,i)
        K = K + 1./2. * m(i) * v(3,i) * v(3,i)
    end do

    ! Potential energy
    U = 0.d0
    do i=1, n_particles
        do j = 1, n_particles
            if (i /= j) then
                dx = x(1,i) - x(1,j)
                dy = x(2,i) - x(2,j)
                dz = x(3,i) - x(3,j)
                rr = sqrt(dx*dx + dy*dy + dz*dz)

                U = U - G*m(j)*m(i)/(rr)
            end if
        end do
    end do
    U = 1./2. * U

    E = K + U
end subroutine calculate_system_energy