PROGRAM Main
    use bmp_comms
    use ip_comms
    
    IMPLICIT NONE
    INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND(12)
    COMPLEX(KIND=dp), ALLOCATABLE :: cw(:,:), cx(:,:), cy(:,:), w(:)
    INTEGER :: size, n, nchann,i,j,mx
    CHARACTER(LEN=10) :: channel_id
    
    call greymap
!
      nchann=30
      READ (nchann,end=99,err=98) size
    ALLOCATE(cx(size,size),cy(size,size),w(size))
    CALL Cffti(w,size)
! Check the FFTs without reordering them.
    do nchann=32,38,2
       write(*,101)nchann
 
       open(nchann,form='unformatted',err=2)
          READ (nchann,end=99,err=98) cx
       CLOSE(nchann)
!
       call quad_transpose(cx,size)
       write(bmname,102)nchann
       call ip_fft_display(size,size,cx)
!
       CALL Cfftb(size,size,cx,cy,w,.True.)
       CALL Cfftb(size,size,cx,cy,w,.false.)
!
       if(nchann.eq.36)call quad_transpose(cx,size)
       write(bmname,100)nchann
       call ip_fft_display(size,size,cx)
!
  2    continue
       write(0,*)'ERROR: Reading channel',nchann
    enddo
!
    stop

    98   write(0,*)'ERROR: Reading channel',nchann
      stop
    99   write(0,*)'ERROR: End of channel',nchann
      stop
100   format(i0.0,'_inverse_FFT')
101   format(/'Reading from fort.',i0.0)
102   format(/i0.0,'_given_FFT')
103   format(32f5.1)

      end
