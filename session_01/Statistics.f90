!!+ Statistics.f90
!!   
!!   This program reads an integer N and N additional real numbers from the input file and
!!   calculates the mean and standard deviation of the real numbers and prints them out to the console
!!   and a text file.
!!
!!   compile with: > gfortran Statistics.f90 -o Statistics
!! 
!!   usage:        > ./Statistics < real100.dat
!!
!!   expected result: console
!!                    0.53604209881462517     0.29218416683921383
!!
!!
!!                    mean_std_result.txt
!!                    Mean:   0.53604209881
!!                    Std:    0.29218416684
!!   
!!-

program statistics
    use constants
    implicit none

    ! local variables
    integer :: N, status, i
    real(dp), dimension (:), allocatable :: input_data_dp, deviation_array_dp
    real(dp) :: mean_dp, std_pop_dp


    READ*, N                                    ! read data_size from first integer in input file
    ALLOCATE(input_data_dp(N), STAT=status)     ! allocate the needed memory to the input_data array
    IF(status/=0) STOP


    ! fill input_data array
    do i = 1, N 
        READ(*, *) input_data_dp(i)   
    end do

    ! calculate mean value
    mean_dp = 1.0/N * sum(input_data_dp)


    ALLOCATE(deviation_array_dp(N), STAT=status) ! allocate memory for the array holding the deviations
    IF(status/=0) STOP
    
    ! fill deviation_array
    do i = 1, N 
        deviation_array_dp(i) = (input_data_dp(i) - mean_dp)**2 
    end do

    
    std_pop_dp = sqrt(1.0/N * sum(deviation_array_dp))       ! calculate population standard deviation as sqrt(variance)
    

    print*, mean_dp, std_pop_dp      ! Print results


    ! open and write to result file
    open(20, file="mean_std_result.txt")
    write(20, '("Mean:", 1X, F15.11, /, "Std:", 2X, F15.11)') mean_dp, std_pop_dp

    
    ! free memory
    deallocate(input_data_dp)
    deallocate(deviation_array_dp) 
end program statistics