!!+ Teaser.f90
!!   
!!   This little program transforms an input string in a certain
!!   way. Try to figure what it does and how it works.
!!
!!   compile with: > gfortran Teaser.f90 -o Teaser
!! 
!!   usage:        > ./Teaser
!!                 Enter text:     Enter a meaningful text here.
!!   
!!-
PROGRAM Teaser
    IMPLICIT NONE
  
    ! local variables
    CHARACTER(LEN=100) :: text
  
  
    ! prompt user for input
    PRINT*, 'Enter text:'
    READ'(A100)', text
  
    ! do something with text
    CALL sort_string(text)
  
    ! print result to screen
    PRINT*, 'text = ', trim(text)
  
  END PROGRAM Teaser
  
  SUBROUTINE sort_string(t)
    IMPLICIT NONE
    
    ! input/ouput variables
    CHARACTER(LEN=*), INTENT(inout) :: t
  
    ! local variables
    INTEGER                         :: nt, i, j
    CHARACTER                       :: tmp
  
    nt = len_trim(t)
  
    DO j=nt, 2, -1
       DO i=1,j-1
          IF (LGT(t(i:i), t(i+1:i+1))) THEN
             tmp        = t(i:i)
             t(i:i)     = t(i+1:i+1)
             t(i+1:i+1) = tmp
          END IF
       END DO
    END DO
  
  END SUBROUTINE sort_string