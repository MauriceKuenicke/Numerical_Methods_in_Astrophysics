!!+ FindPeriod.f90
!!   
!!   This program exports two data files containing N lines with three random numbers each.
!!   The data in one file is calculated using a Linear Congruential Generator (LCG) while the data in the other
!!   is calculated using an already implemented function (random_uniform())
!!   The seed value and LCG parameter can be set to different values if needed.
!!
!!   compile with: > make FindPeriod
!! 
!!   usage:        > ./FindPeriod 
!!
!!   expected result: 
!!                    1  0.400000006    
!!                    2   6.66666701E-02
!!                    3  0.733333349    
!!                    4  0.400000006    
!!                    5   6.66666701E-02
!!                    6  0.733333349    
!!                         ...
!!-

PROGRAM FindPeriod
    USE RandomNumbers_mod
    IMPLICIT NONE
    
    ! local variable declaration
    integer :: i
    
    
    call lcg_init_seed(11)                  ! Set seed value. The seed value is generated from the SYSTEM_CLOCK subroutine if no input is given
    call set_parameter(7, 4, 15)           ! Set new parameter when (a,c,m) is given otherwise use default values in module
   
    ! print out the calculated values 
    do i=1, 15                             ! Here the loop ends at 15 since m=15
       print*, i, lcg_random()
    end do

  
END PROGRAM FindPeriod