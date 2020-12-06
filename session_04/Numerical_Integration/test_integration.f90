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
    real(dp) :: res

    res = trapez(func1, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res
    res = trapez(func2, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res

    res = simpson(func1, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res
    res = simpson(func2, 0._dp, 2._dp, 1e-6_dp)
    print*, "Result: ", res


end program test_integration

