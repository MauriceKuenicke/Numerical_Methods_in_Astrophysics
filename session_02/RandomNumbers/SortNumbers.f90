!!+ SortNumbers.f90
!!   
!!   This program takes N numbers as an input and sorts them accordingly using either
!!   the insertion sort algorithm or the quicksort algorithm. The results get printed if N <= 100 to keep the console clean for very large N when
!!   checking for time complexity. To easily generate N random numbers use the RandomNumbers program.
!!
!!   compile with: > gfortran SortNumbers.f90 -o SortNumbers
!! 
!!   usage:        > ./SortNumbers < numbers.dat or ./RandomNumbers N | ./SortNumbers
!!
!!   expected result: 
!!                 >> sorted array of numbers if N <= 100 <<
!!		
!!-

program SortNumbers
    implicit none

    ! local variables
    integer,parameter :: dp=kind(0.d0)
    integer :: N, i, status
    real(dp), dimension (:), allocatable :: input_data_dp


    READ*, N                                    
    ALLOCATE(input_data_dp(N), STAT=status)     ! allocate the needed memory to the input_data array
    IF(status/=0) STOP
    
    ! Fill  array with input data
    do i = 1, N 
        READ(*, *) input_data_dp(i)   
    end do

    ! Call sorting algorithm
    call insertion_sort(N, input_data_dp)      ! change here between quicksort() and insertion_sort()

    ! Print sorted array if N is not too big
    if (size(input_data_dp) <= 100) then
    print*, input_data_dp
    endif                      

    deallocate(input_data_dp)
end program SortNumbers


subroutine insertion_sort(N, data_array)
    implicit none

    ! Variable declaration
    integer :: N, i, j
    integer,parameter :: dp=kind(0.d0)
    real(dp):: data_array(N), x

    do i = 2, N
        x = data_array(i)
        j = i -1
        do while (j >= 1)
            if (data_array(j) <= x) exit
            data_array(j+1) = data_array(j)
            j = j-1
        end do
        data_array(j+1) = x
    end do
end subroutine insertion_sort


recursive subroutine quicksort(N, data_array)
    implicit none

    ! Variable declaration
    integer :: N
    integer,parameter :: dp=kind(0.d0)
    real(dp):: data_array(N)

    ! local variables
    integer :: left, right, pos
    real(dp) :: pivot, tmp

    if (N > 1) then             ! case that stops the recursion
        pivot = data_array(N-1) ! not optimal --> median element might be a bit better for the performance
        left = 0
        right = N + 1

        do while(left < right)
            right = right - 1
            do while (data_array(right) > pivot)
                right = right - 1
            end do
            left = left + 1
            do while (data_array(left) < pivot)
                left = left + 1
            end do
            if (left < right) then
                tmp = data_array(left)
                data_array(left) = data_array(right)
                data_array(right) = tmp 
            end if
        end do

        if (left == right) then
            pos = left + 1
        else
            pos = left
        end if
        
        call quicksort(pos-1, data_array(:pos-1))   ! run subroutine recursively
        call quicksort(N-pos+1, data_array(pos:))
    end if

end subroutine quicksort