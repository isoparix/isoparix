    SUBROUTINE Cfftf (m, n, c, ch, wa, flag)
! The normal FFT.
        IMPLICIT NONE
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
            write(86,100)iw,wa(iw),iw+k-1,wa(iw+k-1)
            IF (na) THEN
                IF (flag) THEN
                    CALL Passf_x(m,k,l,c,ch,wa(iw:iw+k-1))
                ELSE
                    CALL Passf_y(k,l,n,c,ch,wa(iw:iw+k-1))
                END IF
            ELSE
                IF (flag) THEN
                    CALL Passf_x(m,k,l,ch,c,wa(iw:iw+k-1))
                ELSE
                    CALL Passf_y(k,l,n,ch,c,wa(iw:iw+k-1))
                END IF
            END IF
            na = .NOT. na
            l = 2*l
            iw = iw+k
        END DO
        IF (.NOT. na) c = ch
        if(.not.na)write(86,101)
        if(na     )write(86,102)
    return
100   format('Cfftf: wa between',i6,2f15.7,' and',i6,2f15.7)
101   format('Cfftf: C=CH',/)
102   format('Cfftf: No re-assignment of arrays',/)
    end
