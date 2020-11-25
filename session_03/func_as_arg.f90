!
! this is an example of how to pass a function as an argument
! functions are palced in a separate module to avoid lengthy
! interface blocks in main program
!
! NOTE: this is not a very good example regarding programming style ;)
!
MODULE root

CONTAINS

  FUNCTION fassign(f,z)
    IMPLICIT NONE
    REAL :: fassign, z, f
    
    fassign = f(z)   ! this uses the function in the argument list

  END FUNCTION fassign

END MODULE root

MODULE myfuncs

CONTAINS

  FUNCTION func1(x)
    REAL :: func1,x

    func1 = x*x

  END FUNCTION func1

  FUNCTION func2(x)
    REAL :: func2,x
  
    func2 = sqrt(x)

  END FUNCTION func2

END MODULE myfuncs

PROGRAM test
  USE root
  USE myfuncs
  IMPLICIT NONE
    
  REAL :: x,a

  PRINT*, "x?"
  READ*, x

  a = fassign(func1,x)
  PRINT*, "func1(x) =", a
  a = fassign(func2,x)
  PRINT*, "func2(x) =", a

END PROGRAM test



