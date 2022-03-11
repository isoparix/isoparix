    SUBROUTINE Passf_y (ido, l, m, cc, ch, wa)
! One pass of the normal FFT.
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: ido, l, m
        COMPLEX(8), INTENT(IN)  :: cc(ido,2,l,m), wa(ido)  ! 'ido' was ':' 
        COMPLEX(8), INTENT(OUT) :: ch(ido,l,2,m)
        INTEGER :: i, k

 write(82,100)m,ido,l,2*m*ido*l
        DO k = 1, l
            DO i = 1, ido
                ch(i,k,1,:) =               cc(i,1,k,:)+cc(i,2,k,:)
                ch(i,k,2,:) =DCONJG(wa(i))*(cc(i,1,k,:)-cc(i,2,k,:))
            END DO
        END DO
100 format('Passf_Y: m=',i6,', ido=',i6,', l=',i6,', size=',i8)
    END SUBROUTINE Passf_y
