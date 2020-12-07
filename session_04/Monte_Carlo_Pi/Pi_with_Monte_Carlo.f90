program numerical_pi
    use randomnumbers_mod
    implicit none

    real(kind=8)     :: x_coord, y_coord, pi_estimate, pi_real, error
    integer (kind=8) :: k, i, events, number
    integer (kind =8), dimension(9) :: all_numbers

    all_numbers = (/ 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000/)

    open(unit=33, file='error_pi.dat')

    do k=1,9
        
        number = all_numbers(k)

        events = 0

        do i=1,number
            x_coord = random_uniform(0.0,1.0)
            y_coord = random_uniform(0.0,1.0)

            if (x_coord*x_coord + y_coord*y_coord < 1.0) then
                events = events +1
            end if
        end do

        pi_estimate = 4*real(events)/number

        !print*, pi_estimate

        pi_real = 4.d0*atan(1.d0)

        error = abs(pi_estimate - pi_real)/pi_real

        write(33,*) number, pi_estimate, error
    
    end do


end program numerical_pi