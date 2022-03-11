    SUBROUTINE Times (which)
! If which is not empty, print the times since the previous call.
        CHARACTER(LEN=*), INTENT(IN) :: which
        REAL(8), SAVE :: last_wall = 0.0, last_cpu = 0.0
        REAL(8) :: wall, cpu
        INTEGER :: m, n

        wall = last_wall
        cpu = last_cpu
        CALL SYSTEM_CLOCK(m,n)
!       last_wall = m/REAL(n,KIND=dp)
        last_wall = dfloat(m)/dfloat(n)
        CALL CPU_TIME(last_cpu)
        IF (LEN(which) > 0) THEN
            wall = last_wall-wall
            cpu = last_cpu-cpu
            WRITE (*,'(A," = ",F0.2," seconds, CPU = ",F0.2," seconds")') &
                which,wall,cpu
        END IF
        return
    END
