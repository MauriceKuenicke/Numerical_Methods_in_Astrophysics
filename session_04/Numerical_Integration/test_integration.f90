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
    real(dp) :: res_1, res_2, open_solution_naive
    logical  :: export
    
    export = .false.
    res_1 = trapez(func1, 0._dp, 2._dp, 1e-7_dp, export)
    res_2 = simpson(func1, 0._dp, 2._dp, 1e-7_dp, export)
    write(*, '("Function 1:", 10X, "trapez:  ", F14.12, / ,21X, "simpson: ", F14.12)') res_1, res_2
    
    export = .true.
    res_1 = trapez(func2, 0._dp, 2._dp, 1e-7_dp, export)
    res_2 = simpson(func2, 0._dp, 2._dp, 1e-7_dp, export)
    write(*, '(/,"Function 2:", 10X, "trapez:  ", F14.12, / ,21X, "simpson: ", F14.12)') res_1, res_2
    
    export = .false.
    res_1 = open_solution_naive(func3, 0._dp, 1e-7_dp, "trapez", export)
    res_2 = open_solution_naive(func3, 0._dp, 1e-7_dp, "simpson", export)
    write(*, '(/,"Function 3:", 10X, "trapez:  ", F14.12, / ,21X, "simpson: ", F14.12)') res_1, res_2

end program test_integration


function open_solution_naive(func, lower, accuracy, algorithm, export) result(res)
    use numericalIntegration_mod
    implicit none
    integer, parameter :: dp = kind(0.d0)
    real(dp), external :: func
    character(len=*)   :: algorithm
    real(dp)           ::  j, res_new, error, lower, accuracy, res
    logical  :: export

    j = 1._dp
    res = 1._dp
    res_new = 0._dp
    do  
        if (algorithm == "trapez") then
            res_new = trapez(func, lower, j, accuracy, export)
        end if
        if (algorithm == "simpson") then
            res_new = simpson(func, lower, j, accuracy, export)
        end if
        j = j + 1._dp
        error = abs(res_new - res)
        res = res_new
        if (error <  1e-6_dp) exit
    end do

end function open_solution_naive

