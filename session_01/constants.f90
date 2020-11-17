module constants
    implicit none

    ! Double Precision Byte Size
    integer,parameter :: dp=kind(0.d0)

    ! Constants
    real(dp),parameter :: pi = acos(-1.0_dp) ! calculate Pi for higher precision
    real(dp),parameter :: c  = 299792.458_dp ! speed of light (km*s-1)
    real(dp), parameter :: G = 4*pi**2       ! Gravitational constant (AU^3*yrs^-2*M^-1)

end module constants
