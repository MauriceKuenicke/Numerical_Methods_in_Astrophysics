module MyFuncs

    contains
    
      function func1(x) result(y)
        real(kind=8) :: y,x
        y = x*x-0.5d0
      end function func1

      function dfunc1(x) result(y)
        real(kind=8) :: y,x
        y = 2*x
      end function dfunc1


      function func2(x) result(y)
        real(kind=8) :: y,x
        y = x*x*x-2*x+2 
      end function func2

      function dfunc2(x) result(y)
        real(kind=8) :: y,x
        y = 3*x*x-2 
      end function dfunc2

      function func3(x) result(y)
        real(kind=8) :: y,x
        y = cos(acos(-1.d0)*x) 
      end function func3

      function dfunc3(x) result(y)
        real(kind=8) :: y,x
        y = -1.d0*acos(-1.d0)*sin(acos(-1.d0)*x)
      end function dfunc3
end module MyFuncs