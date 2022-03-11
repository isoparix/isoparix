! The following code started off as Schwarztrauber's, but has been cut
! down to powers of two only, and converted to Fortran 90.
      SUBROUTINE Cffti (wa, n)
! Generate the array of constants.
        IMPLICIT NONE
!       INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12)
        INTEGER, INTENT(IN) :: n
        COMPLEX(8), INTENT(OUT) :: wa(n)
!       COMPLEX(8), INTENT(OUT) :: wa(:)
        INTEGER :: i, k, m, ki
        REAL(16) :: f
        REAL(8)  :: t
     
!       f = (4.0d+00*dATAN(1.0d+00))/dfloat(n)   ! 180 degrees/n
        f = dATAN(1.0d+00)/dfloat(n/8)   !  Denominator is ** EIGHT **!!
        k = 1
        m = n
        DO WHILE (m > 1)
            IF (MOD(m,2) /= 0) THEN
                WRITE (*,*) 'The vector size is not a power of two:', m
                STOP
            END IF
            m = m/2
            DO i = 0, m-1
                t=f*dfloat(i)
                wa(k+i) = CMPLX(DCOS(t),DSIN(t))
                write(80,100)i,k,k+i,t,wa(k+i)
            END DO
            k = k+m
            f = f+f
        END DO
      return
!
100   format('Cffti:',3i8,f15.7,' x',f15.7,',',f15.7)
!
      end
