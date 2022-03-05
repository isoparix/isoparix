      program equalcol_test
c
c      Copyright @ J S Watts IBM UK  1988
c
c       VNET: wattsjs at warvm5, 86695135 at ehone
c
      use isocomm
c
      limit=32
      ixm=10
      iym=16
      iter_lo=0
      iter_hi=limit
c
      allocate(  kdpel(0:limit+1))
      allocate(itermin(0:limit+1))
      allocate(itermax(0:limit+1))
      allocate(  paint(0:limit+1))
      kdpel=0
c
      do i=1,limit,2
         kdpel(i)=10
      enddo
c
  1   continue
      write(*,100)
      read(*,*)ncolindex
      if(ncolindex.le.0)stop
      call equalcol(ncolindex,.false.)
      go to 1
c
      stop
c
100   format('NCOLINDEX?')
c
      end
