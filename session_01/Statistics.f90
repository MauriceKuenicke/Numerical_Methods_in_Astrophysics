program statistics
    use constants
    implicit none

    ! define variables
    integer :: N, status, i
    real(dp), dimension (:), allocatable :: input_data_dp, deviation_array_dp
    real(dp) :: mean_dp, std_pop_dp, std_samp_dp

    ! read data_size from first integer in file
    ! allocate the needed memory for the input_data array
    READ*, N
    ALLOCATE(input_data_dp(N), STAT=status)
    ALLOCATE(deviation_array_dp(N), STAT=status)
    IF(status/=0) STOP

    ! fill input_data array
    do i = 1, N 
        READ(*, *) input_data_dp(i)   
    end do

    ! calculate mean value
    mean_dp = 1.0/N * sum(input_data_dp)

    ! calculate square differences
    do i = 1, N 
        deviation_array_dp(i) = (input_data_dp(i) - mean_dp)**2 
    end do

    ! calculate population standard deviation as sqrt(variance)
    std_pop_dp = sqrt(1.0/N * sum(deviation_array_dp)) 

    ! open  and write to result file
    open(20, file="mean_std_result.txt")
    write(20, '("Mean:", 1X, F15.11, /, "Std:", 2X, F15.11)') mean_dp, std_pop_dp

    ! free memory
    deallocate(input_data_dp)
    deallocate(deviation_array_dp) 
end program statistics