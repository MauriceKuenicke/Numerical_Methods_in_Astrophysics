! A module containing different root finding algorithms
! >> Bisection Algorithm, Newton-Rhapson-Method and Newton-Rhapson-Method for complex functions <<

module RootFinding
    implicit none

    ! new type declaration
    type myroot_type
    real(kind=8) :: x_0, epsilon, y
    integer :: iter
    end type myroot_type

    type myroot_complex_type
    real(kind=8) :: x_val, y_val, irr 
    integer :: iter_number
    end type myroot_complex_type

    
contains


function find_root_bisection(func, a, b, export_accuracy) result(res)
    implicit none
    real(kind=8) :: bracket_start, bracket_end, func, a, b
    type(myroot_type) :: res
    logical :: export_accuracy

    real(kind=8) :: midpoint, func_value_midpoint, diff, func_value_lower, func_value_higher
    integer :: iter

    ! Open export file if data is exported later
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
        
        ! write to export file
        if (export_accuracy) then
            write(20, *) iter, diff
        end if
        
        ! exit loop after 500000 failed iterations to not end up in a infinite loop
        if (iter == 500000) then
            print*, "No root found after 500000 iterations! Giving out last values instead:" 
            exit
        end if
    enddo

    ! declare return variables
    res%x_0 = midpoint
    res%epsilon = diff
    res%y = func(midpoint)
    res%iter = iter

end function find_root_bisection


! Use Newton-Rhapson
function find_root_newton(func, func_derivative, init_guess, export_accuracy) result(res)
    implicit none
    type(myroot_type) :: res
    real(kind=8) :: func, func_derivative, init_guess, func_value, func_value_derivative, epsilon_zero, x
    integer :: iter
    logical :: export_accuracy
    
    ! Open export file if data is exported later
    if (export_accuracy) then
        open(20, file="data/accuracy_newton.dat")
    end if

    epsilon_zero = 1.d0
    x = init_guess
    iter = 0
    do while(abs(epsilon_zero) > 1e-10 )
        func_value = func(x)
        func_value_derivative = func_derivative(x)
        epsilon_zero = -(func_value)/(func_value_derivative)
        x = x + epsilon_zero
        iter = iter+1

        ! write to export file
        if (export_accuracy) then
            write(20, *) iter, abs(epsilon_zero)
        end if

        ! exit loop after 500000 failed iterations to not end up in a infinite loop
        if (iter == 500000) then
            print*, "No root found after 500000 iterations! Giving out last values instead:" 
            exit
        end if
    enddo
    
    ! declare return variables
    res%x_0 = x
    res%epsilon = abs(epsilon_zero)
    res%y = func(x)
    res%iter = iter
end function find_root_newton

! Use Newton-Rhapson Method on a complex function
function find_root_newton_complex(func_complex, func_derivative, init_guess) result(r)
    implicit none
    type(myroot_complex_type) :: r
    complex(kind=8) :: func_complex, func_derivative, init_guess, func_value, func_value_derivative, z, epsilon_zero
    integer :: iter=0
    
    
    epsilon_zero = 1.d0
    z = init_guess
    do while(abs(epsilon_zero) > 1e-10 )
        func_value = func_complex(z)
        func_value_derivative = func_derivative(z)
        epsilon_zero = -(func_value)/(func_value_derivative)
        z = z + epsilon_zero
        iter = iter+1
    end do
    
    ! declare output variables
    r%x_val = real(init_guess)
    r%y_val = imag(init_guess)
    r%iter_number = iter

    ! set irr to 1 if real root z=1 is found
    if (imag(z) == 0.d0) then
        r%irr = 1
    else
        r%irr = 0
    end if
end function find_root_newton_complex

! Calculate the midpoint of an interval
function calc_midpoint(interval_start, interval_end) result(midpoint)
    implicit none
    real(kind=8) :: interval_start, interval_end, midpoint
    
    midpoint = (interval_start + interval_end)/2.

end function calc_midpoint


! Fortran implementation of the linspace function in the python numpy package
! Create an array in a given range with constant stepsizes
subroutine linspace(from, to, array)
    implicit none 
    real(kind=8), intent(in) :: from, to
    real(kind=8), intent(out) :: array(:)
    real(kind=8) :: range
    integer :: n, i
    n = size(array)
    range = to - from

    if (n == 0) return

    if (n == 1) then
        array(1) = from
        return
    end if

    do i=1, n
        array(i) = from + range * (i - 1) / (n - 1)
    end do
end subroutine linspace

    
end module RootFinding
