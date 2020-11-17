module constants
    implicit none

    ! Double Precision Byte Size
    integer,parameter :: dp=kind(0.d0)

    ! Constants
    real(dp), parameter :: pi = acos(-1.0_dp)              ! calculate Pi for higher precision
    real(dp), parameter :: c  = 299792.458_dp              ! speed of light (km*s-1)
    real(dp), parameter :: G = 4.0_dp*pi**2.0_dp           ! Gravitational constant (AU^3*yrs^-2*M^-1)
    real(dp), parameter :: G_m = 6.674e-11_dp              ! Gravitational constant (m^3*kg^-1*s^-2)
    real(dp), parameter :: AU = 1.49598e11_dp              ! Astronomical Unit (m)
    real(dp), parameter :: M_sun = 1.98892e30_dp           ! Mass of the sun (kg)
    real(dp), parameter :: R_sun = 0.00464912633_dp        ! Radius of the sun (AU)

end module constants
