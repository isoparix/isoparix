      subroutine kml_prep
c
c      Prepares KML and grf files from pre-populated grid
c
      use venn_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical nostyle(0:255)   !  True if no style set for this depth
      logical vertline,yalter,xalter
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
      
c
      real(8),dimension(8*(1+nyhi-nylow)) :: xleft,xright,yleft,yright
     *                                      ,xpoint,ypoint,points_array
     *                                      ,xpa,ypa
c
      character(7) colour(0:6)
      character(8) ge_colour
      character(20) graph_name
c
      rgbq=rgbquad
      if(check)write(10,101)(ichar(rgb(mx)),mx=0,1023)
c
      colstep=253./float(maxdepth)
      nostyle=.true.
c
      colour(0)='Black'
      colour(1)='Fuchsia'
      colour(2)='Red'
      colour(3)='Aqua'
      colour(4)='Lime'
      colour(5)='Blue'
      colour(6)='Red'
c
      open(2,file='outcode.kml',form='formatted',status='unknown')
      write(2,1000)                    ! Start of KML file
      idp=huge(idp)
      aleft =-huge(aleft)
      aright= huge(aright)
      alasty=ystart(1)
      np=0
      npolygon=0
      graph_name='grid'
      al=nxlow
      ah=nxhi
      bl=nylow
      bh=nyhi
      if(check.or.graph)call grf_header(al,ah,bl,bh,graph_name)
      do n=1,1+nrle   !  The total number of line elements, plus one 
         if(check)write(10,102)n,idp,depth(n),xstart(n),ystart(n)
     *                                       ,xend  (n),yend  (n)
     *                                       ,aleft,aright,alasty
         if((idp.ne.depth(n).and.np.gt.0)    .or.
     *      xstart(n).gt.aright              .or.
     *      xend  (n).lt.aleft               .or.
     *      (ystart(n)-alasty)**2.gt.1.1     .or.
     *      n.gt.nrle
     *     )then   !   Go around the collected points and remove duplicates
                if(check)write(12,112)idp,depth(n),np,xstart(n)-aright
     *                             ,aleft-xend(n),(ystart(n)-alasty)**2
                npolygon=npolygon+1
                nchann=20+(2*npolygon)
                npair=1
                xpoint(1)=xleft(1)
                ypoint(1)=yleft(1)
c
                do ip=2,np   !  All the left hand side in sequence
                   npair=npair+1
                   xpoint(npair)=xleft(ip)
                   ypoint(npair)=yleft(ip)
                enddo
c
                do ip=np,1,-1   !  All the right hand side in sequence
                   npair=npair+1
                   xpoint(npair)=xright(ip)
                   ypoint(npair)=yright(ip)
                enddo
c
c      CLose the polygon
c
                npair=npair+1
                xpoint(npair)=xpoint(1)
                ypoint(npair)=ypoint(1)
c
c      Remove intermediate points in vertical lines
c
                nx=1
                nq=1
                xpoint(1)=xpoint(1)
                ypoint(1)=ypoint(1)
                vertline=.true.
                do nq=2,npair
                   if(xpoint(nq).ne.xpoint(nx)
     *               )then   !   This point is NOT in a vertical line
                          nx=nx+1
                          vertline=.true.
                      else   !   This point IS in a vertical line
                          if(vertline
     *                      )then
                                 nx=nx+1
                                 vertline=.false.
                          endif
                   endif
                   xpoint(nx)=xpoint(nq)
                   ypoint(nx)=ypoint(nq)
                enddo
                npair=nx
c
c      Remove duplicates
c
                nx=1
                xpoint(1)=xpoint(1)
                ypoint(1)=ypoint(1)
                do nq=2,npair
                   if(xpoint(nq).ne.xpoint(nx).or.
     *                ypoint(nq).ne.ypoint(nx)
     *               )then   !   This point is NOT a duplicate of the last
                          nx=nx+1
                          xpoint(nx)=xpoint(nq)
                          ypoint(nx)=ypoint(nq)
                   endif
                enddo
c               if(check)write(*,*)npair,nx
                npair=nx
c
c               go to 60   !   No anti-jaggies....
c
c      Smooth out the 'jaggies' by merging singleton steps
c
                step=1.0
                xpoint(1+npair)=xpoint(2)
                ypoint(1+npair)=ypoint(2)
                xpoint(2+npair)=xpoint(3)
                ypoint(2+npair)=ypoint(3)
                xpoint(3+npair)=xpoint(4)
                ypoint(3+npair)=ypoint(4)
                xpa(1:npair)=xpoint(1:npair)
                ypa(1:npair)=ypoint(1:npair)
                xa=xpoint(2)
                ya=ypoint(2)
                nx=0
                nadd=1
                do nq=3,npair+1
c
                   sx1=xpoint(nq-1)-xpoint(nq-2)
                   sx2=xpoint(nq  )-xpoint(nq+1)
                   if(sx1*sx2.ge.0
     *               )then
                          yalter=.false.
                      else
                          yalter=.true.
                   endif
c
                   sy1=ypoint(nq-1)-ypoint(nq-2)
                   sy2=ypoint(nq  )-ypoint(nq+1)
                   if(sy1*sy2.ge.0
     *               )then
                          xalter=.false.
                      else
                          xalter=.true.
                   endif
c                         xalter=.false.
c                         yalter=.false.
                   xb=xpoint(nq)
                   yb=ypoint(nq)
                   dx2=((xb-xa)**2)-step
                   dy2=((yb-ya)**2)-step
                   if(((dx2.gt.-0.01.and.dx2.lt.0.01)
     *                .or.
     *                 (dy2.gt.-0.01.and.dy2.lt.0.01))
     *                .and.
     *                 (xalter.or.yalter)
     *               )then   !   This point is 1 ahead of previous
                          nx=nx+nadd
c
                          if(xa.ne.xb
     *                      )then
                                 xpa(nx)=0.5*(xb+xa)
                                 ypa(nx)=yb
                          endif
c
                          if(ya.ne.yb
     *                      )then
                                 xpa(nx)=xb
                                 ypa(nx)=0.5*(yb+ya)
                          endif
c
                          nadd=1
                      else
                          nx=nx+1
                          xpa(nx)=xb
                          ypa(nx)=yb
                          nadd=0
                   endif
                   xa=xb
                   ya=yb
                enddo
                nx=nx+1
                xpa(nx)=xpa(1)      !  Close the smoothed polygon
                ypa(nx)=ypa(1)
                write(nchann,100)'Original',npair
                write(nchann,116)(xpoint(mx),ypoint(mx),mx=1,npair+3)
                write(nchann,100)'Smoothed',nx
                write(nchann,116)(xpa(mx),   ypa(mx),   mx=1,nx)
                call flush(nchann)
                xpoint(1:nx)=xpa(1:nx)
                ypoint(1:nx)=ypa(1:nx)
                npair=nx
 60             continue
c
c      Draw graphs, bitmaps and KML files
c
                if(check)write( 8,104)npolygon
                if(check)write(12,104)npolygon
                if(idp.lt.9999.and.idp.gt.0
     *            )then                  !   Overlays
                       if(nostyle(idp)
     *                   )then
                              write(ge_colour,107)idp
                              ncol=4*ncolstep*idp  !  Range 4*(0 to 255)
                                       write(*,114)ncol
     *                                    ,ichar(rgb(ncol  ))  !  Blue
     *                                    ,ichar(rgb(ncol+1))  !  Green
     *                                    ,ichar(rgb(ncol+2))  !  Red
                              write(2,1004)ge_colour
     *                                    ,ichar(rgb(ncol  ))  !  Blue
     *                                    ,ichar(rgb(ncol+1))  !  Green
     *                                    ,ichar(rgb(ncol+2))  !  Red
                              nostyle(idp)=.false.
                       endif
                       write(ge_colour,107)idp
                       write(2,1001)idp,npolygon,ge_colour
                       do k=1,npair
                          alon=x2lon(xpoint(k))
                          alat=y2lat(ypoint(k))
                          write(2,106)alon,alat,idp*100
                          if(check)write(8,110)idp,np,k,npair
     *                                        ,xpoint(k),ypoint(k)
                       enddo
                       write(2,1002)
                endif
c
                if(check.or.graph
     *            )then
c
c      Draw the polygons on a graph
c
                       nc=modulo(npolygon,7)
                       np=0
                       do k=1,npair
                          np=np+1
                          points_array(np)=xpoint(k)
                          np=np+1
                          points_array(np)=ypoint(k)
                          if(check)write(nchann,106)points_array(np-1)
     *                                    ,points_array(np)
                       enddo
                       call grf_points(npolygon,np,points_array
     *                                ,colour(nc),colour(nc))
                endif
c
                np=0   ! IMPORTANT - reset the point collection index
                idp=depth(n)
c

         endif
c
         if(n.le.nrle
     *     )then
           !  Carry on collecting up all the points for the polygon
                np=np+1
                xleft (np)=xstart(n)-.5  !  Left         (x1
                yleft (np)=ystart(n)-.5  !  Left top     (y1
                xright(np)=xend(n)  +.5  !  Right         (x1
                yright(np)=yend(n)  -.5  !  Right top     (y1
                if(check)write(12,111)xleft (np),yleft (np)
     *                               ,xright(np),yright(np)
     *                               ,depth(n),npolygon
c
                np=np+1
                xleft (np)=xstart(n)-.5  !  Left         (x2
                yleft (np)=ystart(n)+.5  !  Left bottom  (y2
                xright(np)=xend(n)  +.5  !  Right         (x2
                yright(np)=yend(n)  +.5  !  Right bottom  (y2
                if(check)write(12,111)xleft (np),yleft (np)
     *                               ,xright(np),yright(np)
     *                               ,depth(n),npolygon
c
                aleft =xstart(n)
                aright=xend  (n)
                alasty=ystart(n)
         endif
      enddo
c
      if(check.or.graph)call grf_trailer(npolygon)
      write(2,1003)
c
c
c
      return
c
100   format(a8,4i6)
101   format(300(/,4i6))
102   format('Run-line element',i6,', old/new depth',2i6
     *      ,', starts',2f6.1,' ends',2f6.1,', Limits(xl,xr,yl):',3f6.1)
103   format(200i8)
104   format('#Polygon',i3,' depth',i3,' segments',i4)
105   format(i6,2f10.2)
106   format(f0.6,',',f0.6,',',i0)
107   format('depth',i3.3)
109   format(1000(/,f12.5))
110   format('KML_PREP: idp,np,k,npair,xpoint(k),ypoint(k)',4i6,2f20.5)
111   format('KML_PREP: Left:',2f10.1,', Right:',2f10.1,', Depth',2i4)
112   format('KML_PREP - tests:',3i6,' xstart:',f7.1,' xend:',f7.1
     *      ,' ystart:',f7.1)
113   format('KML_PREP - verts: NX',i4,' x(nx):',f7.1,' y(nx):',f7.1
     *                       ,' NQ',i4,' x(nq):',f7.1,' y(nq):',f7.1)
114   format('NCOL:',i4,', Blue:',i4,', Green:',i4,', Red:',i4)
116   format(1000(/,f0.6,',',f0.6))
1000  format(' <kml xmlns="http://www.opengis.net/kml/2.2">'
     *     ,/'  <Document>'
     *      )
1001  format('      <Placemark>'
     *     ,/'         <name>Depth',i3.3,', Polygon',i3.3,'</name>'
     *     ,/'         <styleUrl>#',a8,'</styleUrl>'
     *     ,/'            <Polygon>'
     *     ,/'            <extrude>1</extrude>'
     *     ,/'               <outerBoundaryIs>'
     *     ,/'                  <LinearRing>'
     *     ,/'                     <altitudeMode>'
     *     , 'relativeToGround</altitudeMode>'
     *     ,/'                        <coordinates>'
     *      )
1002  format('                        </coordinates>'
     *     ,/'                  </LinearRing>'
     *     ,/'            </outerBoundaryIs>'
     *     ,/'         </Polygon>'
     *     ,/'      </Placemark>'
     *      )
1003  format('   </Document>'
     *     ,/'</kml>'
     *      )
1004  format('    <Style id="',a8,'">'
     *     ,/'      <LineStyle>'
     *     ,/'        <width>0</width>'
     *     ,/'      </LineStyle>'
     *     ,/'      <PolyStyle>'
     *     ,/'        <color>B0',3z2.2,'</color>'
     *     ,/'      </PolyStyle>'
     *     ,/'    </Style>'
     *      )
      
c
      end
