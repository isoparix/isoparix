      program showpoly
c
      use bmp_comms
c
c      Pprepares bitmap from logfile.txt
c
      implicit real(8) (a-h,o-z)
c
      real(8)   ,allocatable,dimension(:) :: xl,xr
      integer(4),allocatable,dimension(:) :: iy,ncol
      integer(4),dimension(:)  ,allocatable :: ixl,ixr
      character(1),dimension(:,:),allocatable :: grid
      
c
      read(1,*)nline
      allocate(xl  (nline))
      allocate(xr  (nline))
      allocate(iy  (nline))
      allocate(ncol(nline))
      do n=1,nline
         read(*,100)iy(n),xl(n),xr(n),ncol(n)
c        write(*,102)n,iy(n),xl(n),xr(n),ncol(n)
      enddo
      xmax   =maxval(xr)
      xmin   =minval(xl)
      iymax  =maxval(iy)
      iymin  =minval(iy)
      ncolmax=maxval(ncol)
      ncolstep=250/ncolmax
      iybm=iymax-iymin+1
      rxdelta=float(iybm)/(xmax-xmin)
      xl=xl*rxdelta
      xr=xr*rxdelta
      ixmax=0.5+maxval(xr)
      ixmin=0.5+minval(xl)
      ixbm=ixmax-ixmin+1
      allocate(grid(ixmin:ixmax,iymin:iymax))
      do n=1,nline
         ny=iy(n)
         nc=3+(ncol(n)*ncolstep)
         ia=xl(n)+0.5
         ib=xr(n)+0.5
         grid(ia:ib,ny)=char(nc)
      enddo
      call xxstatmap
      bmname='dcsd'
      call array2bmp(ixbm,iybm,grid)
      write(*,101)ixbm,iybm
c
      stop
c
100   format(15x,i5,1x,f11.6,f14.6,i3)
101   format(10i5)
102   format(2i5,2f10.6,i5)
c
      end  
