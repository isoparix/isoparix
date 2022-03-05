PROGRAM FFT_control
!   IMPLICIT NONE
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12)
    COMPLEX(KIND=dp), ALLOCATABLE :: cw(:,:), cx(:,:), cy(:,:), w(:)
    real(8),allocatable :: picture_data(:,:)
    INTEGER :: sizedim, n
    CHARACTER(LEN=100) :: filename

! Read the file name from the argument and read the sizedim.
    IF (COMMAND_ARGUMENT_COUNT() /= 1) THEN
        WRITE (*,'("Wrong number of arguments")')
        STOP
    END IF
    CALL GET_COMMAND_ARGUMENT(1,filename)
    OPEN (42,FILE=filename,ACTION='READ',FORM='UNFORMATTED')
    READ (42)ixdim,iydim
    allocate(picture_data(ixdim,iydim))
    read(42)picture_data
    CLOSE(42)
    sizedim=max0(ixdim,iydim)
    IF (sizedim < 1 .OR. sizedim > 1000000) THEN
        WRITE (*,'("Invalid value of sizedim: ",I0)') sizedim
        STOP
    END IF

! Convert the sizedim to the smallest power of two that will fit, and
! set up the data.
!
      do n=1,16
         if(2**n.ge.sizedim)exit
      enddo
      sizedim=2**n
      rsize=1./dfloat(sizedim**2)
    
    if (n**2 <= sizedim**2/4) n = 2*n
    ALLOCATE(cw(sizedim,sizedim),cx(sizedim,sizedim),cy(sizedim,sizedim),w(sizedim**2))
!
    do ix=1,ixdim
       do iy=1,iydim
          cw(ix,iy)=dcmplx(picture_data(ix,iy),0.0)
       enddo
    enddo


! Time and check the FFTs in single-dimensional mode.

    CALL Cffti(w,sizedim**2)
    cx = cw
    CALL Times('')

! Time and check the FFTs without reordering them.

    CALL Cffti(w,sizedim)
    cx = cw
    CALL Times('')

    write(*,*)'***** 2-D FFT without transposing *****'
    write(*,*)'Original complex input'
    do my=1,sizedim
       write(*,800)cx(:,my)
    enddo

    CALL Cfftf(sizedim,sizedim,cx,cy,w,.True.)
    CALL Cfftf(sizedim,sizedim,cx,cy,w,.False.)

    write(*,*)'Discrete Fourier Transform of input'
    do my=1,sizedim
       write(*,800)rsize*cx(:,my)
    enddo

    CALL Cfftb(sizedim,sizedim,cx,cy,w,.True.)
    CALL Cfftb(sizedim,sizedim,cx,cy,w,.False.)

    write(*,*)'Inverse Transform - original data expected'
    do my=1,sizedim
       write(*,800)rsize*cx(:,my)
    enddo


    CALL Times('2-D FFT without transposing')
    WRITE (*,'("Relative error:",ES9.2)') &
        MAXVAL(ABS(cx/sizedim**2-cw))/MAXVAL(ABS(cw))
        stop
800   format(16f8.2)
        end
