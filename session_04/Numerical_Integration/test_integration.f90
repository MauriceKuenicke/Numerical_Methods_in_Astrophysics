!!+ test_integration.f90
!!
!!  compile with: > gfortran -o test_integration numericalIntegration_mod.f90 myfuncs.f90 test_integration.f90
!!  
!!  usage: > ./test_integration
!!
!!-



program test_integration
    use numericalIntegration_mod
    use myfuncs
    implicit none

    integer, parameter :: dp = kind(0.d0)
    real(dp) :: res, j, res_new, error

    res = trapez(func1, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res
    res = trapez(func2, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res

    res = simpson(func1, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res
    res = simpson(func2, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res
    
    res = 1
    j = 1._dp
    do  
        res_new = simpson(func3, 0._dp, j, 1e-6_dp)
        j = j + 1._dp
        error = abs(res_new - res)
        res = res_new
        if (error <  1e-6_dp) exit
    end do
    print*, "Result: ", res

end program test_integration

