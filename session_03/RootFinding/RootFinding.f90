module RootFinding
    implicit none

    type myroot_type
    real(kind=8) :: x_0, epsilon, y
    integer :: iter
    end type myroot_type

    
contains

function find_root_bisection(func, a, b, export_accuracy) result(res)
    implicit none
    real(kind=8) :: bracket_start, bracket_end, func, a, b
    type(myroot_type) :: res
    logical :: export_accuracy

    real(kind=8) :: midpoint, func_value_midpoint, diff, func_value_lower, func_value_higher
    integer :: iter

    if (export_accuracy) then
        open(20, file="data/accuracy_bisection.dat")
    end if
    
    bracket_start = a
    bracket_end = b
    iter = 0
 
    diff = abs(bracket_start-bracket_end)
    do while (diff > 1e-10)
        midpoint = calc_midpoint(bracket_start, bracket_end)
        func_value_midpoint = func(midpoint)
        func_value_lower = func(bracket_start)
        func_value_higher = func(bracket_end)

        if (func_value_midpoint*func_value_lower <= 0.) then
            bracket_end = midpoint
        else if (func_value_midpoint*func_value_higher <= 0.) then
            bracket_start = midpoint
        endif

        diff = abs(bracket_start-bracket_end)
        iter = iter +1

        if (export_accuracy) then
            write(20, *) iter, diff
        end if

        if (iter == 100000) then
            print*, "No root found after 100000 iterations! Giving out last values instead:" 
            exit
        end if
    enddo

    res%x_0 = midpoint
    res%epsilon = diff
    res%y = func(midpoint)
    res%iter = iter

end function find_root_bisection



function find_root_newton(func, func_derivative, init_guess, export_accuracy) result(res)
    implicit none
    type(myroot_type) :: res
    real(kind=8) :: func, func_derivative, init_guess, func_value, func_value_derivative, epsilon_zero, x
    integer :: iter
    logical :: export_accuracy

    if (export_accuracy) then
        open(20, file="data/accuracy_newton.dat")
    end if

    epsilon_zero = 1
    x = init_guess
    iter = 0
    do while(abs(epsilon_zero) > 1e-10 )
        func_value = func(x)
        func_value_derivative = func_derivative(x)
        epsilon_zero = -(func_value)/(func_value_derivative)
        x = x + epsilon_zero
        iter = iter+1

        if (export_accuracy) then
            write(20, *) iter, abs(epsilon_zero)
        end if

        if (iter == 100000) then
            print*, "No root found after 100000 iterations! Giving out last values instead:" 
            exit
        end if
    enddo

    res%x_0 = x
    res%epsilon = abs(epsilon_zero)
    res%y = func(x)
    res%iter = iter
end function find_root_newton


function calc_midpoint(interval_start, interval_end) result(midpoint)
    implicit none
    real(kind=8) :: interval_start, interval_end, midpoint
    
    midpoint = (interval_start + interval_end)/2.

end function calc_midpoint


    
end module RootFinding
