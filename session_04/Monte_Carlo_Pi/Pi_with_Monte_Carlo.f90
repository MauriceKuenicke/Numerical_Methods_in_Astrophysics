program numerical_pi
    use randomnumbers_mod
    implicit none

    real(kind=dp)         :: x_coord, y_coord, pi_estimate, pi_real, error
    integer               :: k, i, events, number
    integer, dimension(33) :: all_numbers

    all_numbers = (/ 10, 25, 50, 75, 100, 250, 500, 750, 1000, 2500, 5000, 7500, 10000, 25000, 50000, 75000, 100000, 250000, &
    &500000, 750000, 1000000, 2500000, 5000000, 7500000, 10000000, 25000000, 50000000, 75000000, 100000000, &
    &250000000, 500000000, 750000000, 1000000000/)

    open(unit=77, file='error_pi.dat')

    print*, "Started Monte Carlo Integration:"

    do k=1,33
        
        number = all_numbers(k)

        events = 0

        do i=1,number
            x_coord = random_uniform(0.0,1.0)
            y_coord = random_uniform(0.0,1.0)

            if (x_coord*x_coord + y_coord*y_coord < 1.0_dp) then
                events = events +1
            end if
        end do

        pi_estimate = 4_dp*real(events)/real(number)

        !print*, pi_estimate

        pi_real = 4.0_dp*atan(1.0_dp)

        error = abs(pi_estimate - pi_real)/pi_real

        write(77,*) number, pi_estimate, error

        write(*,'(" Finished run ",i2.2," of 33")') k
        write(*,'(" Actual number of random points pi is calculated from is ",E8.2," ;maximum is 0.1E+10 @ run 33")') real(number)
    end do

    print*, "Process Finished"


end program numerical_pi