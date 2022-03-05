      subroutine ge_input
c
c      Read in polygon and cicle co-ordinates
c
c      First two character define type of data
c
      use andrew_comms
c
      implicit real(8) (a-h,o-z)
c
      logical ge_coords,intersector,intersected,convex,boundset
     *       ,lseg_12(400),lseg_21(400),valid
      character(1)  :: space,data_array(10000)
      character(2)  :: data_type
      character(80) :: coordinate 
      character(10000) :: data_txt
      equivalence(data_txt,data_array)
c
      real(8), dimension(4)     :: segment1,segment2
      real(8), dimension(4,400) :: segment_12,segment_21,segment_00
     *                            ,segnm,segig
      real(8), dimension(200) ::
     *                           xpoly,ypoly,epoly,allpoly
     *                          ,segslope_00,segcept_00
     *                          ,segslope_12,segcept_12
     *                          ,segslope_21,segcept_21
c
      notmade=0
      ignored=0
      ndata=0
      totlon=0.
      totlat=0.
      ge_coords=.false.
      boundset=.false.
  1   continue
      read(*,102,end=2)data_type,data_txt
      write(*,104)data_type,data_txt
c
      if(data_type.eq.'ge'
     *  )then
             ndata=ndata+1
             ge_coords=.true.
             read(data_txt,*,end=2)px,py
c            write(*,103)px,py
             totlon=totlon+px
             totlat=totlat+py
             xpoly(ndata)=px
             ypoly(ndata)=py
      endif
c
      if(data_type.eq.'pg'
     *  )then
             ndata=ndata+1
             read(data_txt,*,end=2)xpoly(ndata),ypoly(ndata)
             write(10,103)xpoly(ndata),ypoly(ndata)
      endif
c
      if(data_type.eq.'by'
     *  )then
             boundset=.true.
             read(data_txt,*,end=2)boundary
      endif
c
      go to 1
c
  2   continue
c
      if(.not.boundset
     *  )then
             write(0,111)
             stop
      endif
c
      write(*,109)ndata
c
      if(ge_coords
     *  )then
             rn=1./ndata
             avglon=totlon*rn  ! Average longitude
             avglat=totlat*rn  ! Average latitude
             delta_degree=0.1
             alat=avglat-delta_degree
             blat=avglat+delta_degree
             alon=avglon-delta_degree
             blon=avglon+delta_degree
             radius=3963.2D+00
             deglon2miles=sphere_dist(alon,avglat,blon,avglat,radius)
     *                   /delta_degree
             deglat2miles=sphere_dist(avglon,alat,avglon,blat,radius)
     *                   /delta_degree
             write(*,103)avglon,avglat,deglon2miles,deglat2miles
c
c      Convert lon/lat to cartesian coords in miles centred on averages
c
             write(*,105)
             do n=1,ndata
                xpoly(n)=(xpoly(n)-avglon)*deglon2miles
                ypoly(n)=(ypoly(n)-avglat)*deglat2miles
                write(*,103)xpoly(n),ypoly(n)
             enddo
      endif
c
      px_old=xpoly(1)
      py_old=ypoly(1)
      npstanza=0
      xmax=maxval(xpoly(1:ndata))+boundary
      xmin=minval(xpoly(1:ndata))-boundary
      ymax=maxval(ypoly(1:ndata))+boundary
      ymin=minval(ypoly(1:ndata))-boundary
      call grf_header(xmin,xmax,ymin,ymax)
      allpoly(1)=xpoly(1)
      allpoly(2)=ypoly(1)
      nbase=2
      ns_00=0
      ns_12=0
      ns_21=0
      b2=0.99*(boundary**2)
      do n=2,ndata
         ns_00=ns_00+1
         nbase=nbase+1
         allpoly(nbase)=xpoly(n)
         nbase=nbase+1
         allpoly(nbase)=ypoly(n)
         segment_00(1,ns_00)=xpoly(n-1)
         segment_00(2,ns_00)=ypoly(n-1)
         segment_00(3,ns_00)=xpoly(n)
         segment_00(4,ns_00)=ypoly(n)
         call standoff(boundary
     *   ,segment_00(:,ns_00),segslope_00(ns_00),segcept_00(ns_00)
     *   ,segment1,slope1,cept1,segment2,slope2,cept2
     *                )
c
c      Check whether or not these segment are worth adding.  Maybe their
c      end-points are too close to polygon vertices?
c
         valid=.true.
         do k=1,ndata
            if(pyth2d(segment1(1),segment1(2)
     *               ,xpoly(k),ypoly(k)).lt.b2
     *        )then   !   At least one end is too close...
                   if(pyth2d(segment1(3),segment1(4)
     *                      ,xpoly(k),ypoly(k)).lt.b2
     *               )then   ! So is the other one..
                          valid=.false.
                          write(*,112)segment1,xpoly(k),ypoly(k)
                          notmade=notmade+1
                          segnm(:,notmade)=segment1
                   endif
            endif
         enddo
c
         if(valid
     *     )then
                ns_12=ns_12+1
                 segment_12(:,ns_12)=segment1
                segslope_12(  ns_12)=slope1
                 segcept_12(  ns_12)=cept1
         endif
c
         valid=.true.
         do k=1,ndata
            if(pyth2d(segment2(1),segment2(2)
     *               ,xpoly(k),ypoly(k)).lt.b2
     *        )then   !   At least one end is too close...
                   if(pyth2d(segment2(3),segment2(4)
     *                      ,xpoly(k),ypoly(k)).lt.b2
     *               )then   ! So is the other one..
                          valid=.false.
                          write(*,112)segment2,xpoly(k),ypoly(k)
                          notmade=notmade+1
                          segnm(:,notmade)=segment2
                   endif
            endif
         enddo
c
         if(valid
     *     )then
                ns_21=ns_21+1
                 segment_21(:,ns_21)=segment2
                segslope_21(  ns_21)=slope2
                 segcept_21(  ns_21)=cept2
         endif
c
      enddo
c
      nstanza=1
c
c      Make intersections the end-points of segments
c
      delta=0.01
  3   continue
      m=ns_12
      ntop_12=ns_12
      intersected=.false.
      do n=1,ns_12
         ka=n
            call crossover(segslope_12(m)   ,segcept_12(m)
     *                    ,segslope_12(ka)  ,segcept_12(ka)
     *                    , segment_12(:,m) ,segment_12(:,ka)
     *                    ,x_intercept,y_intercept,intersector)
c
                    xtd=x_intercept-delta*(x_intercept-xpoly(ka))
                    ytd=y_intercept-delta*(y_intercept-ypoly(ka))
                    write(10,1061)intersector,x_intercept,y_intercept
     *                           ,xtd,ytd,xpoly(ka),ypoly(ka)
c
            if(intersector
     *         )then
c
c      Create a point displaced towards the vertex, and see to which end
c      of each of the segments it's closest.
c
                    call segtrim(x_intercept,y_intercept
     *                          ,xtd,ytd,segment_12(:,ka))
                    call segtrim(x_intercept,y_intercept
     *                          ,xtd,ytd,segment_12(:,m))
                    intersected=.true.
                else
                    diff=boundary/sqrt(pyth2d(x_intercept,y_intercept
     *                                        ,xpoly(ka),ypoly(ka)))
                    xc=xpoly(ka)-diff*(xpoly(ka)-x_intercept)
                    yc=ypoly(ka)-diff*(ypoly(ka)-y_intercept)
c
                    segment_12(3,ntop_12+1)=xc
                    segment_12(4,ntop_12+1)=yc
                    segment_12(1,ntop_12+2)=xc
                    segment_12(2,ntop_12+2)=yc
c
                    ntop_12=ntop_12+1
                    call addseg(x_intercept,y_intercept
     *                      ,segment_12(1,ntop_12),segment_12(2,ntop_12)
     *                      ,segment_12(:,ka))
                    call segsc(segment_12(:,ntop_12)
     *                       ,segslope_12  (ntop_12)
     *                       , segcept_12  (ntop_12))
c
                    ntop_12=ntop_12+1
                    call addseg(x_intercept,y_intercept
     *                      ,segment_12(3,ntop_12),segment_12(4,ntop_12)
     *                      ,segment_12(:,m))
                    call segsc(segment_12(:,ntop_12)
     *                       ,segslope_12  (ntop_12)
     *                       , segcept_12  (ntop_12))
            endif
            ka=ka+1
            if(ka.gt.ns_00)ka=1
         m=n
      enddo
c
      m=ns_21
      ntop_21=ns_21
      do n=1,ns_21
         ka=n
            call crossover(segslope_21(m)   ,segcept_21(m)
     *                    ,segslope_21(ka)  ,segcept_21(ka)
     *                    , segment_21(:,m) ,segment_21(:,ka)
     *                    ,x_intercept,y_intercept,intersector)
c
                    xtd=x_intercept-delta*(x_intercept-xpoly(ka))
                    ytd=y_intercept-delta*(y_intercept-ypoly(ka))
                    write(10,1062)intersector,x_intercept,y_intercept
     *                           ,xtd,ytd,xpoly(ka),ypoly(ka)
c
            if(intersector
     *        )then
c
c      Create a point displaced towards the vertex, and see to which end
c      of the segments it's closest.
c
                    call segtrim(x_intercept,y_intercept
     *                          ,xtd,ytd,segment_21(:,ka))
                    call segtrim(x_intercept,y_intercept
     *                          ,xtd,ytd,segment_21(:,m))
                    intersected=.true.
                else
                    diff=boundary/sqrt(pyth2d(x_intercept,y_intercept
     *                                        ,xpoly(ka),ypoly(ka)))
                    xc=xpoly(ka)-diff*(xpoly(ka)-x_intercept)
                    yc=ypoly(ka)-diff*(ypoly(ka)-y_intercept)
c
                    segment_21(3,ntop_21+1)=xc
                    segment_21(4,ntop_21+1)=yc
                    segment_21(1,ntop_21+2)=xc
                    segment_21(2,ntop_21+2)=yc
c
                    ntop_21=ntop_21+1
                    call addseg(x_intercept,y_intercept
     *                      ,segment_21(1,ntop_21),segment_21(2,ntop_21)
     *                      ,segment_21(:,ka))
                    call segsc(segment_21(:,ntop_21)
     *                       ,segslope_21  (ntop_21)
     *                       , segcept_21  (ntop_21))
c
                    ntop_21=ntop_21+1
                    call addseg(x_intercept,y_intercept
     *                      ,segment_21(3,ntop_21),segment_21(4,ntop_21)
     *                      ,segment_21(:,m))
                    call segsc(segment_21(:,ntop_21)
     *                       ,segslope_21  (ntop_21)
     *                       , segcept_21  (ntop_21))
            endif
            ka=ka+1
            if(ka.gt.ns_00)ka=1
c        enddo
         m=n
      enddo
c
c      Clean things up a bit by taking out segments with both ends too
c      near to a vertex
c
      lseg_12=.true.    !   Set all segments as valid
      lseg_21=.true.    !   Set all segments as valid
      b2=0.99*(boundary**2)
      do k=1,ndata
         xp=xpoly(k)
         yp=ypoly(k)
         do n=1,ntop_12
c
            if(lseg_12(n)
     *        )then
                   d1=pyth2d(segment_12(1,n),segment_12(2,n),xp,yp)
                   d2=pyth2d(segment_12(3,n),segment_12(4,n),xp,yp)
c           if(d1.lt.b2.or.d2.lt.b2)write(*,103)d1,d2,b2,d1-b2,d2-b2
                   if(d1.lt.b2
     *               )then
                          if(d2.lt.b2
     *                      )then
                                 write(*,113)segment_12(:,n),xp,yp
                                 lseg_12(n)=.false.
                                 ignored=ignored+1
                                 segig(:,ignored)=segment_12(:,n)
c                            else
c                                segment_12(1,n)=segment_12(3,n)
c                                segment_12(2,n)=segment_12(4,n)
                          endif
c                     else
c                         if(d2.lt.b2
c    *                      )then
c                                segment_12(3,n)=segment_12(1,n)
c                                segment_12(4,n)=segment_12(2,n)
c                         endif
                   endif
            endif
         enddo
c
         do n=1,ntop_21
            if(lseg_21(n)
     *        )then
                   d1=pyth2d(segment_21(1,n),segment_21(2,n),xp,yp)
                   d2=pyth2d(segment_21(3,n),segment_21(4,n),xp,yp)
c           if(d1.lt.b2.or.d2.lt.b2)write(*,103)d1,d2,b2,d1-b2,d2-b2
                   if(d1.lt.b2
     *               )then
                          if(d2.lt.b2
     *                      )then
                                 lseg_21(n)=.false.
                                 write(*,113)segment_21(:,n),xp,yp
                                 ignored=ignored+1
                                 segig(:,ignored)=segment_21(:,n)
c                            else
c                                segment_21(1,n)=segment_21(3,n)
c                                segment_21(2,n)=segment_21(4,n)
                          endif
c                     else
c                         if(d2.lt.b2
c    *                      )then
c                                segment_21(3,n)=segment_21(1,n)
c                                segment_21(4,n)=segment_21(2,n)
c                         endif
                   endif
            endif
c
         enddo
      enddo
      
c
      do n=1,ntop_12
         if(lseg_12(n)
     *     )then
                nstanza=nstanza+1
                call grf_points(0,nstanza,4,segment_12(:,n)
     *                            ,'Red     ','Lime    ')
         endif
      enddo
c
      do n=1,ntop_21
         if(lseg_21(n)
     *     )then
                nstanza=nstanza+1
                call grf_points(0,nstanza,4,segment_21(:,n)
     *                            ,'Blue    ','Red     ')
         endif
      enddo
c
      do n=1,notmade
         nstanza=nstanza+1
         call grf_points(0,nstanza,4,segnm(:,n)
     *                            ,'Yellow  ','Red     ')
      enddo
c
      do n=1,ignored
         nstanza=nstanza+1
         call grf_points(0,nstanza,4,segig(:,n)
     *                            ,'Fuchsia ','Red     ')
      enddo
c
      call grf_points(0,0,nbase,allpoly,'Lime    ','Blue    ')
      call grf_trailer(1+nstanza)
c
100   format(2f10.2,/2f10.2)
101   format(3f20.16)
102   format(a2,a80)
103   format(50f12.6)
104   format('Type: ',a2,', Text: ',a80)
105   format(/)
1061  format(L2,' _12 intersection at',10f12.6)
1062  format(L2,' _21 intersection at',10f12.6)
107   format('Segment testing',8f14.6)
108   format('Repeating intersection testing')
109   format('Number of data points:',i4)
110   format('Intercept:',2f12.6,', Displaced:',2f12.6)
111   format('No boundary given')
112   format('Not creating  segment',4f12.6,'.  Too close to',2f12.6)
113   format('Ignoring segment',4f12.6,'.  Too close to',2f12.6)
c
      return
      end
c
c
c
      subroutine segsc(segment,segslope,segcept)
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: segment
c
      segslope=slope(segment(1),segment(2),segment(3),segment(4))
      segcept =cept (segment(1),segment(2),segment(3),segment(4))
c
      return
      end
c
c
c
c
      subroutine addseg(x_intercept,y_intercept,x,y,segment)
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: segment
c
      if(pyth2d(x_intercept,y_intercept,segment(1),segment(2)).lt.
     *   pyth2d(x_intercept,y_intercept,segment(3),segment(4))
     *  )then
             x=segment(1)
             y=segment(2)
         else
             x=segment(3 )
             y=segment(4 )
      endif
c
      return
c
100   format('x1, y1')
101   format('x2, y2')
c
      end
c
c
c
      subroutine segtrim(x_intercept,y_intercept,xtd,ytd,segment)
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: segment
c
      if(pyth2d(xtd,        ytd,        segment(1),segment(2)).lt.
     *   pyth2d(x_intercept,y_intercept,segment(1),segment(2))
     *  )then
             segment(1)=x_intercept
             segment(2)=y_intercept
         else
             segment(3 )=x_intercept
             segment(4 )=y_intercept
      endif
c
      return
c
100   format('x1, y1')
101   format('x2, y2')
c
      end
c
c
c
      subroutine crossover(slope12,cept12,slope34,cept34
     *               ,s12,s34,x_intercept,y_intercept
     *               ,intersector)
c
c      Do the lines P1-P2 and P3-P4 intersect between P1 and P2 (and
c      between P3 and P4)?
c
c     y=( cept(x3,y3,x4,y4)- cept(x1,y1,x2,y2))
c    * /(slope(x1,y1,x2,y2)-slope(x3,y3,x4,y4))
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: s12,s34
      logical intersector
c
      if(slope12.eq.slope34
     *  )then
             intersector=.false.
             x_intercept=-999.999
             y_intercept=-999.999
             return
       endif
c
      if(slope12.eq.huge(x)
     *  )then
             x_intercept= s12(1)
             y_intercept=(s12(1)*slope34)+cept34
             go to 1
      endif
c
      if(slope34.eq.huge(x)
     *  )then
             x_intercept= s34(1)
             y_intercept=(s34(1)*slope12)+cept12
             go to 1
      endif
c
      if(slope12.eq.0.
     *  )then
             x_intercept=(s12(2)-cept34)/slope34
             y_intercept= s12(2)
             go to 1
      endif
c
      if(slope34.eq.0.
     *  )then
             x_intercept=(s34(2)-cept12)/slope12
             y_intercept= s34(2)
             go to 1
      endif
c
      y_intercept=((cept12*slope34)-(cept34*slope12))
     *            /(slope34-slope12)
      x_intercept=(y_intercept-cept34)/slope34
c
  1   continue
c
      write(10,100)x_intercept,s12(1),s12(3),s34(1),s34(3)
     *            ,y_intercept,s12(2),s12(4),s34(2),s34(4)
      if(
     *   (((x_intercept-s12(1))*(x_intercept-s12(3)).le.0.) .and.
     *    ((x_intercept-s34(1))*(x_intercept-s34(3)).le.0.))
     *    .or.
     *   (((y_intercept-s12(2))*(y_intercept-s12(4)).le.0.).and.
     *    ((y_intercept-s34(2))*(y_intercept-s34(4)).le.0.))
     *  )then
             intersector=.true.
         else
             intersector=.false.
      endif
c
      return
c
100   format(/'CROSSOVER:',5f10.5
     *      ,/'          ',5f10.5)
c
      end
      
      
