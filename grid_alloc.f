      subroutine grid_alloc
c
c      Creates grids and does some other basic allocations
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      logical intext
c
      character(50)filename
c
      lines_per_mile=1.0/miles_per_line
c
      r2max=maxval(r2poly)
c
      inxl=(xmin*lines_per_mile)-5.0
      inxh=(xmax*lines_per_mile)+5.0
      inyl=(ymin*lines_per_mile)-10.0
      inyh=(ymax*lines_per_mile)+10.0
c
      if(check
     *  )then
         write(*,100)xmax,ymax,xmin,ymin
     *              ,inxl,inxh,inyl,inyh
100   format('GRID_ALLOC:  xmax=',f10.5,', ymax=',f10.5
     *                 ,', xmin=',f10.5,', ymin=',f10.5
     *     ,/13x,'inxh=',i10,', inyh=',i10,', inxl=',i10,', inyl=',i10)
      endif
c
      nextra=500
      allocate( ygrid(inyl:inyh))
      allocate(  grid(inxl:inxh,inyl:inyh))
      allocate(zal  (0:2000,inyl:inyh))  !  dim_1 intersections
      allocate(zar  (0:2000,inyl:inyh))  !  dim_1 intersections
      allocate(zbl  (0:2000,inyl:inyh))  !  dim_1 intersections
      allocate(zbr  (0:2000,inyl:inyh))  !  dim_1 intersections
      allocate(zl   (0:2000,inyl:inyh+nextra))     !  dim_1 intersections
      allocate(zr   (0:2000,inyl:inyh+nextra))     !  dim_1 intersections
      allocate(zg   (0:2000,inyl:inyh+nextra))     !  dim_1 intersections
      allocate(agrid(0:2000,inyl:inyh))
      allocate(bgrid(0:2000,inyl:inyh))
      allocate(zcount(inyl:inyh+nextra))!  Number of intersections on line
      allocate(yg    (inyl:inyh+nextra))
c
      grid=0
      agrid=huge(x)
      bgrid=huge(x)
      zg=huge(x)
      nzleft =-1
      nzright=-1
      zcount=0
c
      lcolour(0)='Black'
      lcolour(1)='Fuchsia'
      lcolour(2)='Red'
      lcolour(3)='Aqua'
      lcolour(4)='Lime'
      lcolour(5)='Blue'
      lcolour(6)='Red'
c
      pcolour(1)='Black'
      pcolour(2)='Lime'
      pcolour(3)='Red'
      pcolour(4)='Aqua'
      pcolour(5)='Red'
      pcolour(6)='Black'
      pcolour(0)='Red'
c
      do n=inyl,inyh
         ygrid(n)=miles_per_line*real(n)
      enddo
c
      hugereal=huge(x)
c
      nostyle=.true.
c
c      Open files...
c
      filename=trim(title)//'_grid_scan.txt'
      open(18,file=filename,form='formatted',status='unknown')
      filename=trim(title)//'_grids.txt'
      open(80,file=filename,form='formatted',status='unknown')
      return
      end
