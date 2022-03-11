    SUBROUTINE Passf_x (m, ido, l, cc, ch, wa)
! One pass of the normal FFT.
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: m, ido, l
        COMPLEX(8), INTENT(IN)  :: cc(m,ido,2,l), wa(ido) ! 'ido' was ':'
        COMPLEX(8), INTENT(OUT) :: ch(m,ido,l,2)
        INTEGER :: i, k

        write(82,100)m,ido,l,2*m*ido*l
        DO k = 1, l
            DO i = 1, ido
                ch(:,i,k,1) =               cc(:,i,1,k)+cc(:,i,2,k)
                ch(:,i,k,2) =DCONJG(wa(i))*(cc(:,i,1,k)-cc(:,i,2,k))
            END DO
        END DO
100 format('Passf_X: m=',i6,', ido=',i6,', l=',i6,', size=',i8)
    END SUBROUTINE Passf_x
