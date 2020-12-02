!!+ CalcRoots_Complex.f90
!!   
!!   This program calculates the root of a complex function in a meshgrid from -1 to 1. The function
!!   returns 1 if the real z = 1 is found and 0 otherwise.
!!
!!   compile with: > make CalcRootsComplex
!! 
!!   usage:        > ./CalcRootsComplex
!!
!!   expected result:
!!                  x                         y                         irr                            k
!!                  1.0000000000000000        1.0000000000000000        0.0000000000000000            88414
!!		
!!-

program CalcRoots_Complex
    use MyFuncs
    use RootFinding
    implicit none
    
    ! declare local variables
    type(myroot_complex_type) :: r
    integer :: j , i
    complex(dp) :: init_guess
    real(dp) :: array(100), x_value, y_value

    open(20, file="data/complexnewton.dat")

    ! create array in a specific range with constant stepsize
    call linspace(-1.0_dp, 1.0_dp, array)

    ! iterate over array once for x values and once for y values and try to find the root
    do j=1, size(array)
        x_value = array(j)
        do i=1, size(array)
            y_value = array(i)
            init_guess = cmplx(x_value, y_value, kind=dp)
            r = find_root_newton_complex(func1_complex, dfunc1_complex, init_guess)
            
            write(20, *) r
        end do
    end do
    
    ! write and print results
    print*, "Only showing last result:"
    write(*, '(3X, "x", 25X, "y", 25X, "irr", 28X,"k")')
    print*, r
    print*, "Results exported to data/complexnewton.dat"

end program CalcRoots_Complex

