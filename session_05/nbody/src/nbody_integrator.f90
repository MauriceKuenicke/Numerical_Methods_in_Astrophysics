PROGRAM nbody_integrator
    USE parameters
    USE nbody_io
    USE leapfrog
    IMPLICIT NONE

    ! declare local variables
    integer          :: n_particles
    real(8)          :: time, time_step, time_limit, time_step_half, E
    real(8), dimension (:,:), allocatable :: x, v, a 
    real(8), dimension (:), allocatable :: m

    call load_bodies(n_particles, x, v, a, m)

    time = 0.
    time_step = 0.0002
    time_limit = 10

    time_step_half = time_step*0.5
    open(unit=77, file='out.dat')
    do while(time <= time_limit)
        ! First Position Drift-Step
        call position_drift(n_particles,x,v,time_step_half)

        time = time + time_step_half
        call calculate_force_acceleration(n_particles, m, x, a)

        ! Velocity Kick-Step
        call velocity_kick(n_particles, v,a, time_step)
        
        ! Second Position Drift-Step
        call position_drift(n_particles,x,v,time_step_half)

        time = time + time_step_half

        ! Calculate system energy
        call calculate_system_energy(n_particles, x, v, m, E)

        ! Write results to output file
        write(77,*) time, x, E

    end do
    

end program


subroutine calculate_system_energy(n_particles, x, v, m, E)
    use parameters
    implicit NONE
    integer, intent(in)  :: n_particles
    real(8), intent(in)  :: x(3, n_particles), v(3, n_particles), m(n_particles)
    real(8), intent(out) :: E

    ! Local variables 
    real(8)   :: K, U, dx, dy, dz, rr
    integer   :: i, j

    ! Kinetic energy
    K = 0.d0
    do i=1, n_particles
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