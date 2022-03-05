      subroutine kml_prep
c
c      Prepares KML and grf files from pre-populated grid
c
      use andrew_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical nostyle(0:255)   !  True if no style set for this depth
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
      
c
      character(7) colour(0:6)
      character(8) ge_colour
      character(20) graph_name
c
      rgbq=rgbquad
      write(10,101)(ichar(rgb(mx)),mx=0,1023)
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
      colour(6)='Yellow'
c
      open(2,file='outcode.kml',form='formatted',status='unknown')
      write(2,1000)                    ! Start of KML file
      idp=depth(1)
c     idp=huge(idp)
      nrle_a=1
      npolygon=0
      graph_name='grid'
      call grf_header(xmin,xmax,ymin,ymax,graph_name)
      do n=1,nrle   !  The total number of line elements 
         write(10,102)n,depth(n),xstart(n),ystart(n),xend(n),yend(n)
         if(idp.ne.depth(n)
     *     )then
                npolygon=npolygon+1
                nchann=20+(2*npolygon)
                allocate(points_array(8*(n-nrle_a+1)))
                np=0
c
                points_array(1)=xstart(nrle_a)-.5  !  Left         (x1
                points_array(2)=ystart(nrle_a)-.5  !  Left top     (y1
                points_array(3)=xstart(nrle_a)-.5  !  Left         (x2
                points_array(4)=ystart(nrle_a)+.5  !  Left bottom  (y2
c
                np=4
                do line=nrle_a+1,n-1
                   if(xstart(line).ne.xstart(line-1)
     *               )then                         ! Step across to new x
                          np=np+1
                          points_array(np)=xstart(line)-.5
                          np=np+1
                          points_array(np)=ystart(line)-.5
                          np=np+1
                          points_array(np)=xstart(line)-.5
                          np=np+1
                          points_array(np)=ystart(line)+.5
                      else
                          points_array(np)=xstart(line)-.5
                          points_array(np)=ystart(line)+.5
                   endif
                enddo   
c
                np=np+1
                points_array(np)=xend(n-1)+.5
                np=np+1
                points_array(np)=yend(n-1)+.5
                np=np+1
                points_array(np)=xend(n-1)+.5
                np=np+1
                points_array(np)=yend(n-1)-.5
c
                do line=n-2,nrle_a,-1
                   if(xend(line).ne.xend(line+1)
     *               )then
                          np=np+1
                          points_array(np)=xend(line)+.5
                          np=np+1
                          points_array(np)=yend(line)+.5
                          np=np+1
                          points_array(np)=xend(line)+.5
                          np=np+1
                          points_array(np)=yend(line)-.5
                      else
                          points_array(np)=xend(line)+.5
                          points_array(np)=yend(line)-.5
                   endif
                enddo  
c
                np=np+1
                points_array(np)=xstart(nrle_a)-.5
                np=np+1
                points_array(np)=ystart(nrle_a)-.5
c
                kdp=idp
c               write(nchann,109)(points_array(mx),mx=1,np)
                nc=1+modulo(kdp,7)
c
                ncolstep=4*int(250./float(maxdepth))
                if(kdp.lt.9999.and.kdp.gt.0
     *            )then                  !   Overlays
                       if(nostyle(kdp)
     *                   )then
                              write(ge_colour,107)kdp
                              ncol=ncolstep*kdp
                              write(*,100) ge_colour,ncol
     *                                    ,ichar(rgb(ncol+2))  !  Blue
     *                                    ,ichar(rgb(ncol+1))  !  Green
     *                                    ,ichar(rgb(ncol  ))  !  Red
                              write(2,1004)ge_colour
     *                                    ,ichar(rgb(ncol+2))  !  Blue
     *                                    ,ichar(rgb(ncol+1))  !  Green
     *                                    ,ichar(rgb(ncol  ))  !  Red
                              nostyle(kdp)=.false.
                       endif
                       write(2,1001)kdp,npolygon,ge_colour
                       do k=1,np,2
                          alon=x2lon(points_array(k  ))
                          alat=y2lat(points_array(k+1))
                          write(2,106)alon,alat,kdp*100
                       enddo
                       write(2,1002)
c
c                      write( *,104)npolygon,kdp,2*(n-nrle_a)
                       call grf_points(kdp,np,points_array
     *                                ,colour(2),colour(nc))
                endif
c
                deallocate(points_array)
                idp=depth(n)   !  Start looking for a different depth
                nrle_a=n
         endif
c
      enddo
      write(2,1003)
      call grf_trailer(npolygon)
c
      return
c
100   format(a8,4i6)
101   format(300(/,4i6))
102   format('Run-line element',i6,', depth',i6
     *      ,', starts at',2f10.2,' and ends at',2f10.2)
103   format(200i8)
104   format('#Polygon',i3,' depth',i3,' segments',i4)
105   format(i6,2f10.2)
106   format(f0.6,',',f0.6,',',i0)
107   format('depth',i3.3)
109   format(1000(/,2f12.5))
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
     *     ,/'        <color>80',3z2.2,'</color>'
     *     ,/'      </PolyStyle>'
     *     ,/'    </Style>'
     *      )
      
c
      end
