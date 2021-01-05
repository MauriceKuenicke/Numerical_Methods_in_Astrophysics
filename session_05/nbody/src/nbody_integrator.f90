PROGRAM nbody_integrator
    USE parameters
    USE nbody_io
    USE leapfrog
    IMPLICIT NONE

    ! declare local variables
    integer                                :: n_particles, counter=0
    real(DP)                               :: time, time_step, time_limit, E, E_old, U, T, pertubation
    real(DP), dimension (:,:), allocatable :: x, v, a, a_old 
    real(DP), dimension (:), allocatable   :: m

    ! Parameter
    time = 0.
    time_step = 0.000005
    time_limit = 100
    pertubation = 0.0000_DP
    
    ! Open output file and load body data
    open(unit=77, file='out.dat')
    call load_bodies(n_particles, x, v, a, m, a_old)

    ! Add pertubation
    v(1,:) = v(1,:) + pertubation

    ! Calculate Initial values
    call calculate_force_acceleration(n_particles, m, x, a, U)
    call calculate_kinetic_energy(n_particles, v, m, T)
    E_old = 0._DP
    E = U + T 
    write(77,*) time, x(1,1), x(1,2), x(1,3), x(2,1), x(2,2), x(2,3), x(3,1), x(3,2), x(3,3), E, abs((E - E_old)/E_old)
    
    ! Integrate trajectories
    print*, "Integrate trajectories..."
    do while(time <= time_limit)

        call advance_position(n_particles,x,v,a,time_step) 
        a_old = a
        
        call calculate_force_acceleration(n_particles, m, x, a, U)

        call update_velocities(n_particles, v,a, a_old, time_step)
        time = time + time_step
        counter = counter + 1 

        call calculate_kinetic_energy(n_particles, v, m, T)
        E = U + T

        ! Write results to output file
        write(77,*) time, x(1,1), x(1,2), x(1,3), x(2,1), x(2,2), x(2,3), x(3,1), x(3,2), x(3,3), E, abs((E - E_old)/E_old)

        ! Output current time and partial energy error for every 100000 steps
        if (mod(counter,100000) == 0) then
            write(*, '(" Timestep:   ", F9.6, 10X, "Partial Energy Error:  ", E9.2)') time, abs((E - E_old)/E_old)
        end if
        E_old = E
    end do
    print*, "Done!"
end program


subroutine calculate_kinetic_energy(n_particles, v, m, T)
    use parameters
    implicit NONE
    integer, intent(in)  :: n_particles
    real(8), intent(in)  :: v(n_particles, 3), m(n_particles)
    real(8), intent(out) :: T

    ! Local variables
    integer   :: i

    T = 0.d0
    do i=1, n_particles
        T = T + m(i) *(v(i,1)*v(i,1)+v(i,2)*v(i,2)+v(i,3)*v(i,3))
    end do
    T = 0.5_DP*T
end subroutine calculate_kinetic_energy