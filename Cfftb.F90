    SUBROUTINE Cfftb (m, n, c, ch, wa, flag)
! The inverse FFT.
        IMPLICIT NONE
!       INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12)
        INTEGER, INTENT(IN) :: m, n
        COMPLEX(8), INTENT(INOUT) :: ch(m,n), c(m,n)
        COMPLEX(8), INTENT(IN) :: wa(m)  !  Dimension was ':'
        LOGICAL, INTENT(IN) :: flag
        INTEGER :: mn, k, l, iw
        LOGICAL :: na

        na = .True.
        iw = 1
        l = 1
        IF (flag) THEN
            mn = n
        ELSE
            mn = m
        END IF
        k = mn
        DO WHILE (l < mn)
           k = k/2
           write(80,*)'Cfftb: wa between',iw,' and',iw+k-1
            IF (na) THEN
                IF (flag) THEN
                    CALL Passb_x(m,k,l,c,ch,wa(iw:iw+k-1))
                ELSE
                    CALL Passb_y(k,l,n,c,ch,wa(iw:iw+k-1))
                END IF
            ELSE
                IF (flag) THEN
                    CALL Passb_x(m,k,l,ch,c,wa(iw:iw+k-1))
                ELSE
                    CALL Passb_y(k,l,n,ch,c,wa(iw:iw+k-1))
                END IF
            END IF
            na = .NOT. na
            l = 2*l
            iw = iw+k
        END DO
        IF (.NOT. na) c = ch
        if(.not.na)write(80,101)
        if(na     )write(80,102)
      return
101   format('Cfftb: C=CH',/)
102   format('Cfftb: No re-assignment of arrays',/)
      end
