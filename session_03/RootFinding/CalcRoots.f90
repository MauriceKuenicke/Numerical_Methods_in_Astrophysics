program CalcRoots
    use MyFuncs
    use RootFinding
    implicit none

    type(myroot_type) :: r
    logical :: export_accuracy

    export_accuracy = .TRUE.   ! .FALSE. or .TRUE.
    r = find_root_newton(func2, dfunc2, -5.d0, export_accuracy)   ! find_root_newton(func, func_derivative, init_guess) or find_root_bisection(func, a, b)
    !r = find_root_bisection(func2, -10.d0, 10.d0, export_accuracy)   
    
    write(*, '(2X, "Root", 23X, "Epsilon", 19X, "Value", 28X,"Iterations")')
    print*, r
    

end program CalcRoots