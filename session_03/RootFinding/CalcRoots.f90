program CalcRoots
    use MyFuncs
    use RootFinding
    implicit none

    type(myroot_type) :: r

    r = find_root_newton(func3, dfunc3, 2.1d0)   ! find_root_newton(func, func_derivative, init_guess) or find_root_bisection(func, a, b)
    !r = find_root_bisection(func2, 0.d0, 2.9d0)   
    
    write(*, '(2X, "Root", 23X, "Epsilon", 19X, "Value", 28X,"Iterations")')
    print*, r
    

end program CalcRoots