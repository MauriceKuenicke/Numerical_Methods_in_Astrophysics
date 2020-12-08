!!+ test_integration.f90
!!  Calculate the integral of three test functions using the trapezoidal and Simpson's rule. Use a naive solution
!!  for integrals with no upper bound. Export the results for Function 2 into the /data directory.
!!
!!  compile with: > gfortran -o test_integration numericalIntegration_mod.f90 myfuncs.f90 test_integration.f90
!!  
!!  usage: > ./test_integration
!!
!!  expected result:
!!                  Function 1:          trapez:  6.000000447035
!!                                       simpson: 6.000000298023
!!
!!                  Function 2:          trapez:  8.153364636096
!!                                       simpson: 8.153364464001
!!                     
!!                  Function 3:          trapez:  0.499999955293
!!                                       simpson: 0.499999985091
!!-



program test_integration
    use numericalIntegration_mod
    use myfuncs
    implicit none

    ! Declaration of local variables
    integer, parameter :: dp = kind(0.d0)
    real(dp) :: res_1, res_2, open_solution_naive
    logical  :: export
    
    ! Calculate results for test function 1
    export = .false.
    res_1 = trapez(func1, 0._dp, 2._dp, 1e-7_dp, export)         ! trapez(function, lower_bound, upper_bound, accuracy, export)
    res_2 = simpson(func1, 0._dp, 2._dp, 1e-7_dp, export)        ! simpson(function, lower_bound, upper_bound, accuracy, export)
    write(*, '("Function 1:", 10X, "trapez:  ", F14.12, / ,21X, "simpson: ", F14.12)') res_1, res_2
    
    ! Calculate and export results for test function 2
    export = .true.
    res_1 = trapez(func2, 0._dp, 2._dp, 1e-7_dp, export)
    res_2 = simpson(func2, 0._dp, 2._dp, 1e-7_dp, export)
    write(*, '(/,"Function 2:", 10X, "trapez:  ", F14.12, / ,21X, "simpson: ", F14.12)') res_1, res_2
    
    ! Calculate results for test function 3
    export = .false.
    res_1 = open_solution_naive(func3, 0._dp, 1e-7_dp, "trapez", export)
    res_2 = open_solution_naive(func3, 0._dp, 1e-7_dp, "simpson", export)
    write(*, '(/,"Function 3:", 10X, "trapez:  ", F14.12, / ,21X, "simpson: ", F14.12)') res_1, res_2

end program test_integration


function open_solution_naive(func, lower, accuracy, algorithm, export) result(res)
    use numericalIntegration_mod
    implicit none

    ! Declaration of local virables
    integer, parameter :: dp = kind(0.d0)
    real(dp), external :: func
    character(len=*)   :: algorithm
    real(dp)           :: upper_bound, res_new, error, lower, accuracy, res
    logical            :: export

    ! Initialize values
    res = 1._dp
    res_new = 0._dp

    ! Increase upper bound j until changes in area are smaller than the given accuracy
    upper_bound = 1._dp
    do  
        if (algorithm == "trapez") then
            res_new = trapez(func, lower, upper_bound, accuracy, export)
        end if
        if (algorithm == "simpson") then
            res_new = simpson(func, lower, upper_bound, accuracy, export)
        end if
        upper_bound = upper_bound + 1._dp

        error = abs((res_new - res)/res_new)
        res = res_new
        if (error <  1e-6_dp) exit               ! exit loop if desired accuracy is achieved
    end do

end function open_solution_naive

