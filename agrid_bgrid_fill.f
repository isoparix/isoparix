      subroutine polyfill
c
c      Populates grid from centres are located in xp and yp.
c      LGRID is a grid of logical values, the same size as GRID
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      logical intext
c
      if(check)write(*,100)ncentres
c
      rd=1./dgrid           !   Reciprocal of grid resolution
      dy=0.5D+00*dgrid
c
      lgrid=.true.
      do ntype=1,2   !  Inner, then outer, radii
         do nc=1,ncentres    !   FOR loop over all the 'supermarket centres'
            if(ntype.eq.1
     *        )then
                   r=r1p(nc)
               else
                   r=r2p(nc)
            endif
            rad=r**2
            nyl=-.5+((yp(nc)-r)*rd) ! Highest possible Y for this circle
            nyh= .5+((yp(nc)+r)*rd) ! Lowest  possible Y for this circle
            do iy=nyl,nyh      !  FOR loop over these values
               ya=rad-(ygrid(iy)-dy-yp(nc))**2
               y2=rad-(ygrid(iy)   -yp(nc))**2
               yb=rad-(ygrid(iy)+dy-yp(nc))**2
c
c      Record real start and end points of this line.
c
               if(ya.ge.0.0
     *           )then   !   We have real roots
                      xla=(xp(nc)-dsqrt(ya))*rd 
                      xha=(xp(nc)+dsqrt(ya))*rd 
                      if(yb.lt.0.0
     *                  )then
                             xlb=xla
                             xhb=xha
                             nxl=xla
                             nxh=xha
                      endif
               endif
c
               if(yb.ge.0.0
     *           )then   !   We have real roots
                      xlb=(xp(nc)-dsqrt(yb))*rd 
                      xhb=(xp(nc)+dsqrt(yb))*rd 
                      if(ya.lt.0.0
     *                  )then
                             xla=xlb
                             xha=xhb
                             nxl=xlb
                             nxh=xhb
                      endif
               endif
c
               if(ya.ge.0.0.and.yb.ge.0.0
     *           )then   !  We have real roots top and bottom
                      nxh=int(dmax1(xha,xhb))
                      nxl=int(dmin1(xla,xlb))
               endif
c
               if(ya.ge.0.0.or.yb.ge.0.0
     *           )then   !  We have real roots either top and bottom
                      agrid(nxl,iy)=xla
                      agrid(nxh,iy)=xha
                      bgrid(nxl,iy)=xlb
                      bgrid(nxh,iy)=xhb
c
c      For outer radii, the information about their boundaries need to
c      to be given to their 'outside' neighbours as well, so +/- 1 on the 
c      X-coordinates.
c
                      agrid(nxl-1,iy)=xla
                      agrid(nxh+1,iy)=xha
                      bgrid(nxl-1,iy)=xlb
                      bgrid(nxh+1,iy)=xhb
c
                      if(check)write(80,104)nxl,nxh,iy
     *                                     ,agrid(nxl,iy),agrid(nxh,iy)
     *                                     ,bgrid(nxl,iy),bgrid(nxh,iy)
c
                      do ix=nxl,nxh  !  FOR loop between (including) roots
                         if(lgrid(ix,iy)
     *                     )then
                                lgrid(ix,iy)=.false.  ! No more action
                                if(ntype.eq.2
     *                            )then    !   Outside
                                       if(intext(xgrid(ix),ygrid(iy))
     *                                    .and.ncentres.gt.2 ! Minimum for poly
     *                                   )then    !   Inside...
                                              grid(ix,iy)=grid(ix,iy)+4 !Inside
                                          else
                                              grid(ix,iy)=grid(ix,iy)+1 !Outside
                                       endif
                                endif
                         endif
                      enddo
               endif   !  Real roots check
            enddo   ! iy=nyl,nyh
         enddo  ! nc=1,ncentres
      enddo   ! ntype=1,2
c
         maxdepth=maxval(grid)   !   Maximum value of the whole grid
         if(check)write(*,105)maxdepth
c
      return
100   format('POLYFILL: ncentres=',i6)
101   format('POLYFILL: nc,nxl,nxh,nyl,nyh,xp,yp',5i5,2f10.5)
102   format('POLYFILL: ix,iy,rad1,d2,rad',2i5,3f10.5)
103   format('POLYFILL: xp,yp',2f10.5)
104   format('POLYFILL: nxl,nxh,iy,agrid(*,iy)',3i5,2e25.16
     *     ,/'                     bgrid(*,iy)',15x,2e25.16)
105   format('POLYFILL: All complete.   Highest grid value now',i4)

      end
