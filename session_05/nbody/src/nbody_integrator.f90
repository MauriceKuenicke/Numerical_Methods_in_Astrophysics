PROGRAM nbody_integrator
    USE parameters
    USE nbody_io
    USE leapfrog
    IMPLICIT NONE

    ! declare local variables
    integer          :: n_particles, counter=0
    real(8)          :: time, time_step, time_limit, E, E_old, U, T
    real(8), dimension (:,:), allocatable :: x, v, a, a_old 
    real(8), dimension (:), allocatable :: m
    
    open(unit=77, file='out.dat')
    call load_bodies(n_particles, x, v, a, m, a_old)

    time = 0.
    time_step = 0.0001
    time_limit = 10
    E_old = 0._DP
    
    ! Calculate Initial values
    call calculate_force_acceleration(n_particles, m, x, a, U)
    call calculate_kinetic_energy(n_particles, v, m, T)
    E = U + T 
    write(77,*) time, x(1,1), x(1,2), x(1,3), x(2,1), x(2,2), x(2,3), x(3,1), x(3,2), x(3,3), E, abs((E - E_old)/E_old)
    do while(time <= time_limit)

        call leapfrog_part1(n_particles,x,v,a,time_step) 
        a_old = a
        
        call calculate_force_acceleration(n_particles, m, x, a, U)

        call leapfrog_part2(n_particles, v,a, a_old, time_step)
        time = time + time_step
        counter = counter +1 

        call calculate_kinetic_energy(n_particles, v, m, T)
        E = U + T

        ! Write results to output file
        write(77,*) time, x(1,1), x(1,2), x(1,3), x(2,1), x(2,2), x(2,3), x(3,1), x(3,2), x(3,3), E, abs((E - E_old)/E_old)
        if (mod(counter,1000) == 0) then
            print*, time, abs((E - E_old)/E_old)
        end if
        E_old = E


    end do
    

end program


subroutine calculate_kinetic_energy(n_particles, v, m, T)
    use parameters
    implicit NONE
    integer, intent(in)  :: n_particles
    real(8), intent(in)  :: v(n_particles, 3), m(n_particles)
    real(8), intent(out) :: T

    ! Local variables
    integer   :: i

    ! Kinetic energy
    T = 0.d0
    do i=1, n_particles
        T = T + m(i) *(v(i,1)*v(i,1)+v(i,2)*v(i,2)+v(i,3)*v(i,3))
    end do
    T = 0.5_DP*T
end subroutine calculate_kinetic_energy