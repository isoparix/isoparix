      program andrew_venn
c
c      Associate an area on a grid with an individual circle
c      Repeat for as many times as we have circles
c      Add 1 for each time a grid point is inside a circle
c      Create polygons from equal value edges
c      Convert polygon edges from x,y pairs to long/lat pairs
c
      implicit real(8) (a-h,o-z)
c
      real(8),dimension(100) :: radius,xcentre,ycentre
c
      integer(4),dimension(:,:),allocatable :: grid
      integer(4),dimension(:)  ,allocatable :: depth
      real(8),   dimension(:)  ,allocatable :: xstart,ystart,xend,yend
     *                                        ,points_array
      logical(1),dimension(:)  ,allocatable :: line_view
      logical(1) end_segment
      character(7) colour(0:6)
c
      colour(0)='Black'
      colour(1)='Fuchsia'
      colour(2)='Red'
      colour(3)='Aqua'
      colour(4)='Lime'
      colour(5)='Blue'
      colour(6)='Yellow'
      
c
      xmin=1000000.
      xmax=0.
      ymin=1000000.
      ymax=0.
      rmin=1000000.
      ncircles=0
  1   continue
      read(1,*,end=2,err=99)r,cx,cy
      ncircles=ncircles+1
c     write(0,101)ncircles,r,cx,cy
c
      if(cx+r.gt.xmax)xmax=cx+r+2.
      if(cy+r.gt.ymax)ymax=cy+r+2.
      if(cx-r.lt.xmin)xmin=cx-r-2.
      if(cy-r.lt.ymin)ymin=cy-r-2.
      if(r.lt.rmin)rmin=r
      radius (ncircles)=r
      xcentre(ncircles)=cx
      ycentre(ncircles)=cy
c     write(0,101)ncircles,radius(ncircles)
c    *           ,xcentre(ncircles),ycentre(ncircles)
      go to 1
  2   continue
c
      read(*,*)resolution
c
      dgrid=rmin/resolution
      nxlow=xmin/dgrid
      nxhi =xmax/dgrid
      nylow=ymin/dgrid
      nyhi =ymax/dgrid
      xmin=nxlow
      xmax=nxhi
      ymin=nylow
      ymax=nyhi
      nelements=(nxhi-nxlow+1)*(nyhi-nylow+1)
      nsegments=10*(nyhi-nylow)
      write(0,*)dgrid,nxlow,nxhi,nylow,nyhi,nelements
      allocate(line_view(nylow:nyhi))
      allocate(grid(nxlow:nxhi,nylow:nyhi))
      allocate(depth (nsegments))
      allocate(xstart(nsegments))
      allocate(ystart(nsegments))
      allocate(  xend(nsegments))
      allocate(  yend(nsegments))
      grid=0
c
      do nc=1,ncircles
         rad2=radius(nc)**2
         do iy=nylow,nyhi
            ydiff=rad2-((dgrid*float(iy))-ycentre(nc))**2
            if(ydiff.gt.0.0
     *        )then
                   do ix=nxlow,nxhi
                      x2=((dgrid*float(ix))-xcentre(nc))**2
                      if(x2.lt.ydiff
     *                  )then
                             grid(ix,iy)=grid(ix,iy)+1
                      endif
                   enddo
            endif
         enddo
      enddo
c
c     do iy=nylow,nyhi
c        write(8,100)iy,(grid(mx,iy),mx=nxlow,nxhi)
c        call isoflush(8)
c     enddo
c
      ncount=0
      kx=-1
      kxmax=0
      line_view=.true.
      nrle=0
      kx_old=-3
  3   continue
      do iy=nylow,nyhi
         if(line_view(iy)
     *     )then
c
                do ix=nxlow,nxhi
                   if(grid(ix,iy).ne.-1
     *               )then
                          kx=grid(ix,iy)             
                          idx=ix
                          if(kx.gt.kxmax)kxmax=kx
                          nrle=nrle+1
                          depth(nrle)=kx
                          xstart(nrle)=float(ix)
                          ystart(nrle)=float(iy)
                          exit
                   endif
                enddo
c
                do ix=idx,nxhi
                   if(grid(ix,iy).eq.kx
     *               )then
                          xend(nrle)=float(ix)
                          yend(nrle)=float(iy)
                          grid(ix,iy)=-1       ! Clear this element
                          ncount=ncount+1      ! How many have we cleared?
                      else
                          exit
                   endif
                enddo                   ! End of line
c
         endif
         if(grid(nxhi,iy).eq.-1
     *     )then
                line_view(iy)=.false.   ! All elemnts of this line cleared
         endif
      enddo
c
      nrle=nrle+1
      depth(nrle)=999
      xstart(nrle)=999
      ystart(nrle)=999
c
      write(8,103)ncount,nelements
      do iy=nylow,nyhi
         write(8,100)iy,(grid(mx,iy),mx=nxlow,nxhi)
         call isoflush(8)
      enddo
c
      write(*,103)ncount,nelements
      if(ncount.lt.nelements
     *  )then
             go to 3
      endif
c
      idp=depth(1)
      nrle_a=1
      npolygon=0
      call grf_header(xmin,xmax,ymin,ymax)
      do n=1,nrle
         write(10,102)depth(n),xstart(n),ystart(n),xend(n),yend(n)
         if(idp.ne.depth(n)
     *     )then
                npolygon=npolygon+1
                nchann=20+(2*npolygon)
                allocate(points_array(8*(n-nrle_a+1)))
                np=0
c
                do line=nrle_a,n-1
                   np=np+1
                   points_array(np)=xstart(line)-.5
                   np=np+1
                   points_array(np)=ystart(line)-.5
                   np=np+1
                   points_array(np)=xstart(line)-.5
                   np=np+1
                   points_array(np)=ystart(line)+.5
                enddo   
c
                do line=n-1,nrle_a,-1
                   np=np+1
                   points_array(np)=xend(line)+.5
                   np=np+1
                   points_array(np)=yend(line)+.5
                   np=np+1
                   points_array(np)=xend(line)+.5
                   np=np+1
                   points_array(np)=yend(line)-.5
                enddo  
c
                   np=np+1
                   points_array(np)=xstart(nrle_a)-.5
                   np=np+1
                   points_array(np)=ystart(nrle_a)-.5
c
                   nc=1+modulo(idp,7)
c
                if(idp.lt.999
     *            )then
                       write( *,104)npolygon,idp,2*(n-nrle_a)
                       call grf_points(idp,np,points_array
     *                                ,colour(2),colour(nc))
                endif
                deallocate(points_array)
                idp=depth(n)
                nrle_a=n
         endif
c
      enddo
      call grf_trailer(npolygon)
c
      stop
c
 99   continue
      stop
c
100   format(i4,' ',2000i1)
101   format(i6,3f6.2)
c102   format(i6,' ends, and',i6,' begins at Y=',i6,', X=',f6.2)
102   format(i6,4f10.2)
103   format(200i6)
104   format('#Polygon',i3,' depth',i3,' segments',i4)
105   format(i6,2f10.2)
c
      end
