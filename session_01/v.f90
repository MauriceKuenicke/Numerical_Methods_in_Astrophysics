!! Old Version
!!PROGRAM v2
!!    do i=0,100
!!    x=0.00464912633+i*(39.43850267 -0.00464912633)&
!!    &/100.0;y=1.49598 e11*x;z=sqrt (6.673e -11*1.98892 e30/y)
!!    w=2*3.1415*y;t=w/z
!!    print*, y, z, t
!!enddo
!!end

!! New Version
!!+ v.f90
!!   
!!   This program calculates the orbital distance, velocity and period of an object around the Sun, in this case Pluto,
!!   over a given timeframe and prints them out at every timestep. 
!!
!!   compile with: > gfortran v.f90 -o v
!! 
!!   usage:        > ./v 
!!
!!   expected result: console
!!                    695500000.71534002        436870.97438265308        10002.851280777455     
!!                    59687756224.974777        47158.407617775636        7952542.3328737859     
!!                    118680012449.23422        33443.595886146424        22296899.921153832     
!!                    177672268673.49365        27333.291608531774        40842054.590823740
!!                    ....
!!                    5722944353753.8818        4816.0611998095847        7466333666.7595654     
!!                    5781936609978.1416        4791.4294250077228        7582075395.9659653     
!!                    5840928866202.4004        4767.1717631086440        7698409089.5169315     
!!                    5899921122426.6602        4743.2788388180597        7815331750.3861828 
!!   
!!-

program v
    use constants
    implicit none
    
    ! local variables
    integer :: timestep, timeframe
    real(dp) :: distance_au_dp, distance_m_dp, velocity_dp, orbit_circumference_dp, orbital_period_dp
    real(dp) :: planet_distance_dp

    
    planet_distance_dp = 39.43850267_dp     ! Set distance for Pluto
    timeframe = 100                         ! Set timeframe

    ! Calculate distance, velocity and period
    do timestep=0, timeframe
        distance_au_dp = R_sun + timestep * (planet_distance_dp - R_sun)/timeframe  ! Distance in AU at every timestep 
        distance_m_dp = AU * distance_au_dp                                         ! Distance in m
        velocity_dp = sqrt(G_m * M_sun/distance_m_dp)                               ! Orbital Velocity
        orbit_circumference_dp = 2 * pi * distance_m_dp                             ! Orbit Circumference
        orbital_period_dp = orbit_circumference_dp/velocity_dp                      ! Orbital Period

        print*, distance_m_dp, velocity_dp, orbital_period_dp
    end do
    
end program v  
