! A module containing the test functions that will be used to test the numerical integration methods

module MyFuncs
    implicit none
    private
    integer, parameter :: dp = kind(0.d0)
    real(dp), parameter :: pi = acos(-1._dp)

    public :: func1
    public :: func2
    public :: func3

    contains

    function func1(x) result(y)
        real(dp) :: x, y

        y = x*(1+x*x)

    end function func1


    function func2(x) result(y)
        real(dp) :: x, y

        y = x**4 * log(x + sqrt(1+x*x))
    end function func2

    function func3(x) result(y)
        real(dp) :: x, y
        y = exp(-pi*(x*x))
    end function func3


end module MyFuncs