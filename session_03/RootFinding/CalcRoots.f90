program CalcRoots
    use MyFuncs
    use RootFinding
    implicit none

    type(myroot_type) :: r

    r = find_root_newton(func1, dfunc1, 0.5d0)   ! find_root_newton(func, func_derivative, init_guess) or find_root_bisection(func, a, b)
    !r = find_root_bisection(func1, -5.d0, 5.d0)   
    
    write(*, '(2X, "Root", 23X, "Epsilon", 19X, "Value", 28X,"Iterations")')
    print*, r
    

end program CalcRoots