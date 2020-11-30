program CalcRoots_Complex
    use MyFuncs
    use RootFinding
    implicit none

    integer :: r, j , i
    complex(kind=8) :: init_guess
    real(kind=8) :: array(100), x_value, y_value

    r = 0
    call linspace(-1.d0, 1.d0, array)
    do j=1, size(array)
        x_value = array(j)
        do i=1, size(array)
            y_value = array(i)
            init_guess = cmplx(x_value, y_value, kind=8)
            r = find_root_newton_complex(func1_complex, dfunc1_complex, init_guess)
        end do
    end do

    !print*, r    

end program CalcRoots_Complex

