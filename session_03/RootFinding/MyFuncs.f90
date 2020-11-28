module MyFuncs

    contains
    
      function func1(x) result(y)
        real(kind=8) :: y,x
        y = 3*x-2
      end function func1

      function dfunc1(x) result(y)
        real(kind=8) :: y,x
        y = 3*x
      end function dfunc1


      function func2(x) result(y)
        real(kind=8) :: y,x
        y = sin(x)
      end function func2

end module MyFuncs