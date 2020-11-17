!!+ PlanetPeriods.f90
!!   
!!   This program calculates the orbital periods our planets of the solar system based on the input data
!!   given in PlanetData.dat and prints them out to the console and into a datafile. The calculated output 
!!   will then be plotted on logarithmic axis with gnuplot.
!!
!!   compile with: > gfortran PlanetPeriods.f90 -o PlanetPeriods
!! 
!!   usage:        > ./PlanetPeriods
!!
!!   expected result: stdout
!!                    Planet Mercury is at a distance of  0.39au from the Sun and has an orbital period of   0.2435549 years.
!!                    Planet Venus   is at a distance of  0.72au from the Sun and has an orbital period of   0.6109403 years.
!!                    Planet Earth   is at a distance of  1.00au from the Sun and has an orbital period of   1.0000000 years.
!!                    Planet Mars    is at a distance of  1.52au from the Sun and has an orbital period of   1.8739819 years.
!!                    Planet Jupiter is at a distance of  5.10au from the Sun and has an orbital period of  11.5174216 years.
!!                    Planet Saturn  is at a distance of  9.54au from the Sun and has an orbital period of  29.4660935 years.
!!                    Planet Uranus  is at a distance of 19.18au from the Sun and has an orbital period of  83.9987657 years.
!!                    Planet Neptun  is at a distance of 30.06au from the Sun and has an orbital period of 164.8099639 years.
!!
!!
!!                    period.dat
!!                    0.39000000000000001       0.24355492193753753     
!!                    0.71999999999999997       0.61094025894517701     
!!                    1.0000000000000000        1.0000000000000000     
!!                    1.5200000000000000        1.8739818569025688     
!!                    5.0999999999999996        11.517421586448938     
!!                    9.5399999999999991        29.466093463504791     
!!                    19.180000000000000        83.998765657597616     
!!                    30.059999999999999        164.80996394635852     
!!
!!
!!                    distancePeriod.png
!!   
!!-



program PlanetPeriods
    use constants
    implicit none

    ! local variables
    real(dp) :: distance(8)
    real(dp) :: orbital_period
    character(7) :: planet(8)
    integer :: i

    ! fill arrays from input data
    open(10, file="PlanetData.dat", status="old")
    do i = 1, 8 
        read(10, *) planet(i), distance(i)   
    end do

    ! open file to store results
    open(20, file="period.dat")

    ! calculate orbital periods for every planet
    do i = 1, 8
        orbital_period = 2*pi*sqrt((distance(i)**3)/G)                                                 
        write(*, '("Planet", 1X, A7, " is at a distance of", 1X, F5.2,"au from the Sun and &
        &has an orbital period of ", F11.7, 1X, "years.")') planet(i), distance(i), orbital_period     ! write output to stdout
        write(20, *) distance(i), orbital_period                                                       ! write data to period.dat
    end do

    ! call Gnuplot to plot the data
    call execute_command_line('gnuplot -p plot.plt')
end program PlanetPeriods
