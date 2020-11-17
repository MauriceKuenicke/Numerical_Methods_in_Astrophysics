!PROGRAM v2
!    do i=0,100
!    x=0.00464912633+i*(39.43850267 -0.00464912633)&
!    &/100.0;y=1.49598 e11*x;z=sqrt (6.673e -11*1.98892 e30/y)
!    w=2*3.1415*y;t=w/z
!    print*, y, z, t
!enddo
!end


program v
    use constants
    implicit none
    
    ! create variables
    integer :: timestep, timeframe
    real(dp) :: distance_au_dp, distance_m_dp, velocity_dp, orbit_circumference_dp, orbital_period_dp
    real(dp) :: planet_distance_dp

    planet_distance_dp = 39.43850267_dp
    timeframe = 100

    do timestep=0, timeframe
        distance_au_dp = R_sun + timestep * (planet_distance_dp - R_sun)/timeframe  ! Distance in AU at every timestep 
        distance_m_dp = AU * distance_au_dp                                         ! Distance in m
        velocity_dp = sqrt(G_m * M_sun/distance_m_dp)                               ! Orbital Velocity
        orbit_circumference_dp = 2 * pi * distance_m_dp                             ! Orbit Circumference
        orbital_period_dp = orbit_circumference_dp/velocity_dp                      ! Orbital Period

        print*, distance_m_dp, velocity_dp, orbital_period_dp
    end do
    
end program v  
