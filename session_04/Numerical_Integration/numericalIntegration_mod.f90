! Module containing the trapezoidal and simpson's rule for numerical integrations. 
module numericalIntegration_mod
    implicit none
    private

    ! Declaration of module variables
    integer, parameter :: dp = kind(0.d0)
    real(dp)           :: trapezoid_area_old
    integer            :: recursion_level

    ! Make functions public
    public :: trapez
    public :: simpson


contains

! Trapezoidal Rule for numerical integration
function trapez(func, lower, upper, accuracy, export) result(trapezoid_area_new)
    ! Declaration of local variables
    real(dp), external   :: func
    real(dp)             :: lower, upper, accuracy, h, relative_error, trapezoid_area_new, h_new
    integer              :: evals=0
    logical              :: export
    
    ! Initialize variables
    recursion_level = 0
    trapezoid_area_old = 0

    ! Calculate initial h and area
    h = upper - lower
    trapezoid_area_old = 1._dp/2._dp * h * (func(lower) + func(upper))

    ! Open file if export variable is set to true
    if (export) then
        open(20, file="data/accuracy_trapez.dat")
    end if


    do  
        h_new = (1._dp/2._dp)** recursion_level * h                                           ! Reduce step size by a factor of two
        trapezoid_area_new = trapez_next(func, h_new, lower, evals)                           ! Calculate next trapezoid area estimate
        relative_error = abs((trapezoid_area_new - trapezoid_area_old)/trapezoid_area_new)    ! Calculate relative error
        trapezoid_area_old = trapezoid_area_new                                               ! Set new estimate to be the old estimate for the next iteration
        
        ! Export
        if (export) then
            write(20, *) evals, relative_error
        end if

        ! Exit loop if desired accuracy level is achieved or increase recursion_level by one
        if (relative_error <= accuracy) then
            exit
        else
            recursion_level = recursion_level + 1
        end if
    end do
end function trapez

! Function that calculates the next area estimate based on a given step size
function trapez_next(func, h_new, lower, evals) result(trapezoid_area_new)
    ! Declaration of local variables
    real(dp), external      :: func
    integer                 :: i, nr_trapezoids
    real(dp)                :: lower, h_new, trapezoid_area_new, area
    integer, intent(out)    :: evals                                      ! Give back the number of function evaluations for this level
    
    nr_trapezoids = 2**(recursion_level)                                  ! Calculate the number of trapezoids

    ! Calculate the summation term
    area = 0                                                              ! Reset area
    do i = 1, nr_trapezoids, 2
        area = area + func(lower+i*h_new)
        evals = evals + 1
    end do

    ! Calculate the new trapezoid area using the old value
    trapezoid_area_new = (1._dp/2._dp)*trapezoid_area_old + h_new * area
end function trapez_next

! Simpson's rule for numerical integration
function simpson(func, lower, upper, accuracy, export) result(area)
    ! Declaration of local variables
    real(dp), external      :: func
    real(dp)                :: area, lower, upper, accuracy, h, t_k, t_kplus, h_new, relative_error
    integer                 :: evals=0
    logical                 :: export
    
    
    ! Open file if export variable is set to true
    if (export) then
        open(20, file="data/accuracy_simpson.dat")
    end if
    
    ! Initialize initial variables
    recursion_level = 0                                                     ! Set initial recursion level
    trapezoid_area_old = 0                                                  ! Set initial trapezoid area
    h = upper - lower
    trapezoid_area_old = 1._dp/2._dp * h * (func(lower) + func(upper))      ! Calculate initial  trapezoid area
    h_new = (1._dp/2._dp)** recursion_level * h                             ! Calculate firest step size reduction
    t_k = trapez_next(func, h_new, lower, evals)                            ! Calculate first extra step using the trapezoid rule
    trapezoid_area_old = t_k

    ! use Simpson's rule to calculate the area estimate based on the trapezoid rule
    do  
        trapezoid_area_old = t_k
        
        ! Increase recursion level by one
        recursion_level = recursion_level + 1
        h_new = (1._dp/2._dp)** recursion_level * h
        
        t_kplus = trapez_next(func, h_new, lower, evals)
        
        area = (4._dp/3._dp)*t_kplus - (1._dp/3._dp)*t_k
        relative_error = abs((area - trapezoid_area_old)/area)

        if (export) then
            write(20, *) evals, relative_error
        end if

        ! Exit loop if desired accuracy is achieved
        if(relative_error < accuracy) exit
        t_k = t_kplus                                              ! Set the newly calculated trapez area as the old area for the next iteration
    end do 
end function simpson

end module numericalIntegration_mod