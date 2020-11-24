!!+ bug02.f90
!!   
!!   This little program is supposed to compute the determinant
!!   of a user-supplied 3x3 matrix (in a rather inefficient way btw 
!!   but that is not the bug)
!!
!!   compile with: > gfortran bug02.f90 -o bug02
!! 
!!   usage:        > ./bug02
!!                  enter 3x3 matrix:
!!                  1 4 7 2 5 8 3 0 9
!!
!!   expected result:
!!                  1.00000        2.00000        3.00000     
!!                  4.00000        5.00000        0.00000
!!                  7.00000        8.00000        9.00000
!!                  determinant is =   -36.000000000000000    
!!		
!!   However, a little bug has sneaked into the program, and you will
!!   need to fix it before you will get the correct result.
!!   
!!-
PROGRAM determinant
  IMPLICIT NONE

  ! local variables
  REAL(KIND=8), DIMENSION(3,3) :: a
  INTEGER                      :: n, i, j
  REAL(KIND=8)                 :: deta

  ! function requires an explicit definition of an interface
  ! because the array is passed with an assumed shape
  ! the interface block consists of the declaration part of
  ! the function (only in/out variables)
  INTERFACE  
     FUNCTION det(a) RESULT(d)
       REAL(KIND=8), DIMENSION(:,:), INTENT(in) :: a
       REAL(KIND=8)                             :: d
     END FUNCTION det
  END INTERFACE

  ! ask user for 3x3 matrix and confirm input
  n = 3
  PRINT '(A,I0,"x",I0,A)', 'enter ',n,n,' matrix:'
  READ*, a

  PRINT*, 'matrix entered:'
  DO i=1,n
     WRITE(*,*) (a(i,j),j=1,n)
  END DO

  ! compute determinant and print result
  deta = det(a)
  PRINT*, 'determinant is = ', deta
END PROGRAM determinant

!
! function det(a) computes the determinant of a
!
FUNCTION det(a) RESULT(d)
  IMPLICIT NONE
  REAL(KIND=8), DIMENSION(:,:), INTENT(in) :: a
  REAL(KIND=8)                             :: d, p
  INTEGER                                  :: n, i, j
  INTEGER, DIMENSION(:), ALLOCATABLE       :: ind

  ! initialize result and get array size in 1d
  d = 0.d0
  n = size(a,1)

  ! allocate ind array and set nw-se diagonal
  ALLOCATE(ind(2*n-1))
  ind = (/ (i,i=1,n), (i,i=1,n) /) ! Changed the second part of ind from (i,i=1,n-1) to (i,i=1,n) to make the index run over every matrix element
  PRINT*, ind

  ! add up nw-to-se diagonals
  DO i=1, n
     p = 1.d0
     DO j=1,n
        p = p*a(j,ind(i+j))
     ENDDO
     d = d + p
     !PRINT*, d
  ENDDO

  ! set ne-sw diagonal
  ind = (/ (i,i=n,1,-1), (i,i=n,1,-1) /) ! Changed the ind variable as before to the same as the first part of the index that every matrix element is considered: from (i,i=n,2,-1) to (i,i=n,1,-1)

  ! subtract ne-to-sw diagonals
  DO i=1, n
     p = 1.d0
     DO j=1,n
        p = p*a(j,ind(i+j))
     ENDDO
     d = d - p
  ENDDO

  DEALLOCATE(ind)

END FUNCTION det

