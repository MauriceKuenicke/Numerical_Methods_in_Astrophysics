!!+ CalcRoots.f90
!!   
!!   This program calculates the root of a given function by either using the bisection algorithm
!!   or the Newton-Rhapson method. In some cases the algorithms get stuck. When this happens the loop breaks
!!   and a message >>"No root found after 500000 iterations! Giving out last values instead:"<< is returned.
!!
!!   compile with: > make CalcRoots
!! 
!!   usage:        > ./CalcRoots
!!
!!   expected result:
!!                  Root                       Epsilon                   Value                            Iterations
!!                 -1.7692923542386314        2.3913279714982673E-014   0.0000000000000000                8
!!		
!!-

program CalcRoots
    use MyFuncs
    use RootFinding
    implicit none
    
    ! declare locale variables
    type(myroot_type) :: r
    logical :: export_accuracy


    export_accuracy = .TRUE.                                          ! .FALSE. or .TRUE.  --> no data will be exported if set to .FALSE.

    
    ! r = find_root_newton(func2, dfunc2, -5.d0, export_accuracy)     ! find_root_newton(func, func_derivative, init_guess)
    r = find_root_bisection(func2, -10.d0, 10.d0, export_accuracy)    ! find_root_bisection(func, a, b)
    
    ! write and print result
    write(*, '(2X, "Root", 23X, "Epsilon", 19X, "Value", 28X,"Iterations")')
    print*, r
    

end program CalcRoots