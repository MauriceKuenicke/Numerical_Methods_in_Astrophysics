program PlanetPeriods
    use constants
    implicit none

    real(dp) :: distance(8)
    real(dp) :: orbital_period
    character(25) :: planet(8)
    integer :: i

    ! Create arrays from input data
    open(10, file="PlanetData.dat", status="old")
    do i = 1, 8 
        read(10, *) planet(i), distance(i)   
    end do

    ! for every  array element (planet, distance)
    ! calculate orbital period
    ! print result to stdout
    open(20, file="period.dat")
    do i = 1, 8
        orbital_period = 2*pi*sqrt((distance(i)**3)/G)
        write(*, '("Planet", 1X, A7, " is at a distance of", 1X, F5.2,"au from the Sun and &
        &has an orbital period of ", F11.7, 1X, "years.")') planet(i), distance(i), orbital_period
        write(20, *) distance(i), orbital_period
    end do
    call execute_command_line('gnuplot -p plot.plt')
end program PlanetPeriods
