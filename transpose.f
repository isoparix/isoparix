      subroutine transpose (array, n)
c
c This code is transposition in place, using the obvious code for a
c square matrix, and gives lots of scope for tuning.
c
      INTEGER, INTENT(IN) :: n
      COMPLEX(8), INTENT(INOUT) :: array(n,n)
      INTEGER :: i, j
      COMPLEX(8) :: x
c
      DO i = 2,n
         DO j = 1,i-1
            x = array(i,j)
            array(i,j) = array(j,i)
            array(j,i) = x
         END DO
       END DO
c
      return
      END
