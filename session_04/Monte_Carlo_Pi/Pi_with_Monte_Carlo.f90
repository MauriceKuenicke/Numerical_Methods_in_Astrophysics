!!+ Pi_with_Monte_Carlo.f90
!!
!!     This program calculates Pi by integration according to the Monte Carlo method.
!!     Additionally the relative deviation to Pi is determined and written to a file (error_pi.dat)
!!     with the number of the used random numbers and the value of Pi.
!!     With the file "error_pi.plt" a plot can be generated from this data, which shows the relative deviation
!!     over the number of random numbers used.
!!     The number of random numbers has been set to an array of powers of ten with starting value 10 with quarter steps.
!!     The expected runtime of the program is in the range of a few seconds up to one minute,
!!     depending on the calculation power of the computer used, since up to 500 000 000 random numbers are used for determining Pi.
!!     
!!     
!!
!!     compile with: > gfortran -O2 -Wall -c Pi_with_Monte_Carlo.f90
!!                     gfortran -O2 -Wall RandomNumbers_mod.o Pi_with_Monte_Carlo.o -o Pi_with_Monte_Carlo
!!
!!     usage:        > ./Pi_with_Monte_Carlo
!!
!!     expected result:
!!                     >> Started Monte Carlo Integration:
!!                        Finished run 01 of 31
!!                        ...
!!                        Finished run 31 of 31
!!                        Process finished
!!
!!                        Output to file:
!!                        This line and the next line is not part of the file and is only given for clarity.
!!                        random points | estimated value of pi | realitve deviation
!!                        Generated error_pi.dat file begins now:
!!                               10   3.5999999046325684   1.4591555990525015E-01
!!                        ...
!!                        250000000   3.1415123939514160   2.5547436356967020E-05
!!                        500000000   3.1415076255798340   2.7065256172525408E-05
!!-

program numerical_pi
    use randomnumbers_mod
    implicit none

    real(kind=dp)           :: x_coord, y_coord, pi_estimate, error
    integer                 :: k, i, events, number, seed
    integer, dimension(31)  :: all_numbers
    
    !Parameters to calculate a value of Pi as accurate as possible 
    real(kind=dp), parameter:: pi_real = 4.0_dp*atan(1.0_dp)

    !Array for specifying the amount of random numbers to be used for the Monte Carlo method for calculating Pi.
    all_numbers = (/ 10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 10000, 25000, 50000, 75000, 100000, 250000, &
    &500000, 750000, 1000000, 2500000, 5000000, 7500000, 10000000, 25000000, 50000000, 75000000, 100000000, &
    &250000000, 500000000 /)

    !Setting the seed value for the generation of reproducible results.
    seed = init_random_seed(26574)

    open(unit=77, file='error_pi.dat')

    print*, "Started Monte Carlo Integration:"

    do k=1,31
        
        number = all_numbers(k)

        events = 0

        do i=1,number
            !Produces two random numbers.
            x_coord = random_uniform(0.0,1.0)
            y_coord = random_uniform(0.0,1.0)

            !Check if an random number pair is inside the circle.
            if (x_coord*x_coord + y_coord*y_coord < 1.0_dp) then
                events = events +1
            end if
        end do

        !Estimation of pi by dividing the number of points within the quarter circle by all generated points.
        !The multiplication by 4 is needed, because the generated random numbers are limited to the first quadrant.
        !This could be changed by altering the interval limits with random_uniform() to -1 to 1.
        pi_estimate = 4_dp*real(events)/real(number)

        !Error formular given by lab session 4.
        error = abs(pi_estimate - pi_real)/pi_real

        write(77,'(I9,3X,F18.16,3X,ES22.16)') number, pi_estimate, error

        write(*,'(" Finished run ",i2.2," of 31")') k

    end do

    print*, "Process Finished"


end program numerical_pi