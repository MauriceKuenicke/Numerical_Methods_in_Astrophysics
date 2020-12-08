module numericalIntegration_mod
    implicit none
    private

    ! Double precision byte size
    integer, parameter :: dp = kind(0.d0)
    real(dp)           :: trapezoid_area_old
    integer            :: recursion_level


    public :: trapez
    public :: simpson


contains

function trapez(func, lower, upper, accuracy, export) result(trapezoid_area_new)
    real(dp), external   :: func
    real(dp)             :: lower, upper, accuracy, h, relative_error, trapezoid_area_new, h_new
    integer              :: evals
    logical  :: export
    
    recursion_level = 0
    trapezoid_area_old = 0
    h = upper - lower
    trapezoid_area_old = 1._dp/2._dp * h * (func(lower) + func(upper))   ! Calculate inital estimate

    if (export) then
        open(20, file="data/accuracy_trapez.dat")
    end if

    do  
        h_new = (1._dp/2._dp)** recursion_level * h
        trapezoid_area_new = trapez_next(func, h_new, lower, evals)
        relative_error = abs((trapezoid_area_new - trapezoid_area_old)/trapezoid_area_new)
        trapezoid_area_old = trapezoid_area_new
        

        if (export) then
            write(20, *) evals, relative_error
        end if
        if (relative_error <= accuracy) exit
        recursion_level = recursion_level + 1
    end do

end function trapez

function trapez_next(func, h_new, lower, evals) result(trapezoid_area_new)
    real(dp), external :: func
    integer            :: i, nr_trapezoids
    real(dp)           :: lower, h_new, trapezoid_area_new, area
    integer, intent(out) :: evals
    
    nr_trapezoids = 2**(recursion_level)
    area = 0
    do i = 1, nr_trapezoids, 2
        area = area + func(lower+i*h_new)
        evals = evals + 1
    end do
    trapezoid_area_new = (1._dp/2._dp)*trapezoid_area_old + h_new * area

end function trapez_next

function simpson(func, lower, upper, accuracy, export) result(area)
    real(dp), external      :: func
    real(dp)                :: area, lower, upper, accuracy, h, t_k, t_kplus, h_new, relative_error
    integer :: evals
    logical  :: export

    recursion_level = 0
    trapezoid_area_old = 0
    h = upper - lower

    if (export) then
        open(20, file="data/accuracy_simpson.dat")
    end if
    
    trapezoid_area_old = 1._dp/2._dp * h * (func(lower) + func(upper))
    h_new = (1._dp/2._dp)** recursion_level * h
    t_k = trapez_next(func, h_new, lower, evals)
    trapezoid_area_old = t_k
    do  
        trapezoid_area_old = t_k

        recursion_level = recursion_level + 1
        h_new = (1._dp/2._dp)** recursion_level * h
        
 
        t_kplus = trapez_next(func, h_new, lower, evals)
        
        area = (4._dp/3._dp)*t_kplus - (1._dp/3._dp)*t_k
        relative_error = abs((area - trapezoid_area_old)/area)

        if (export) then
            write(20, *) evals, relative_error
        end if
        if(relative_error < accuracy) exit
        t_k = t_kplus
    end do 

end function simpson

end module numericalIntegration_mod