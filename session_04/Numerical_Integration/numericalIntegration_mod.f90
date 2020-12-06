module numericalIntegration_mod
    implicit none
    private

    ! Double precision byte size
    integer, parameter :: dp = kind(0.d0)
    real(dp)           :: trapezoid_area_old
    integer            :: recursion_level




    public :: trapezoidal


contains

function trapezoidal(func, lower, upper, accuracy) result(trapezoid_area_new)
    real(dp), external   :: func
    real(dp)             :: lower, upper, accuracy, h, relative_error, trapezoid_area_new
    
    recursion_level = 0
    trapezoid_area_old = 0
    h = upper - lower
    trapezoid_area_old = 1._dp/2._dp * h * (func(lower) + func(upper))   ! Calculate inital estimate
    do
        trapezoid_area_new = trapez_next(func, h, lower)
        relative_error = abs((trapezoid_area_new - trapezoid_area_old)/trapezoid_area_new)
        trapezoid_area_old = trapezoid_area_new
        if (relative_error <= accuracy) exit
    end do

end function trapezoidal

function trapez_next(func, h, lower) result(trapezoid_area_new)
    real(dp), external :: func
    integer            :: i, nr_trapezoids
    real(dp)           :: lower, h, h_new, trapezoid_area_new, area
    
    h_new = (1._dp/2._dp)** recursion_level * h
    nr_trapezoids = 2**(recursion_level)
    area = 0
    do i = 1, nr_trapezoids, 2
        area = area + func(lower+i*h_new)
    end do
    recursion_level = recursion_level + 1
    trapezoid_area_new = (1._dp/2._dp)*trapezoid_area_old + h_new * area

end function trapez_next



end module numericalIntegration_mod