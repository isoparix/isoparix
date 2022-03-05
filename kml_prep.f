      subroutine kml_prep(np,npolygon,ndepth)
c
c      Prepares KML and grf files from supplied open polygon
c
      use venn_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical vertline,unaltered,polytest
c
      character(1)rgb(0:1023)
      character(1024)rgbq

      equivalence(rgb,rgbq)
c
      real(8),dimension(2*((2*np)+1)) :: points_array
      real(8),dimension((2*np)+1) :: xpoint,ypoint,xpa,ypa
c
      character(18)polygonCoords
c
      rgbq=rgbquad
c     if(check)write(10,101)(ichar(rgb(mx)),mx=0,1023)
c
      colstep=253./float(maxdepth)
      nostyle=.true.
c
      nchann=20+(2*npolygon)
      npair=0
      do ip=1,np   !  All the left hand side in sequence
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
c      Close the polygon
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
     *     )then   !   This point is NOT in a vertical line
                nx=nx+1
                vertline=.true.
            else   !   This point IS in a vertical line
                if(vertline
     *            )then
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
     *      ypoint(nq).ne.ypoint(nx)
     *     )then   !   This point is NOT a duplicate of the last
                nx=nx+1
                xpoint(nx)=xpoint(nq)
                ypoint(nx)=ypoint(nq)
            else
                if(check
     *            )then
                       write(18,120)nq,xpoint(nq),ypoint(nq)
                endif
         endif
      enddo
      npair=nx
c
c      Draw graphs, bitmaps and KML files
c
      if(nostyle(ndepth)
     *  )then
             write(ge_colour,107)ndepth
             ncol=4*ncolstep*ndepth  !  Range 4*(0 to 255)
             write(2,1004)ge_colour
     *                   ,ichar(rgb(ncol  ))  !  Blue
     *                   ,ichar(rgb(ncol+1))  !  Green
     *                   ,ichar(rgb(ncol+2))  !  Red
             nostyle(ndepth)=.false.
      endif
      write(ge_colour,107)ndepth
      write(2,1001)ndepth,npolygon,ge_colour
      write(polygonCoords,119)ndepth,npolygon
      write(4,2001)polygonCoords
      do k=1,npair
         alon=x2lon(xpoint(k))
         alat=y2lat(ypoint(k))
         write(2,106)alon,alat,ndepth*100
         write(4,2002)alat,alon
         if(check)write(8,110)ndepth,np,k,npair
     *                       ,xpoint(k),ypoint(k)
      enddo
      write(2,1002)
      write(4,2003)npolygon,polygonCoords
     *            ,ichar(rgb(ncol+2))  !  Red
     *            ,ichar(rgb(ncol+1))  !  Green
     *            ,ichar(rgb(ncol  ))  !  Blue
     *                      ,npolygon
c
      if(check.or.graph
     *  )then
c
c      Draw the polygons on a graph
c
             nb=modulo(ndepth,7)
             nc=modulo(npolygon,7)
             npq=0
             do k=1,npair
                npq=npq+1
                points_array(npq)=xpoint(k)
                npq=npq+1
                points_array(npq)=ypoint(k)
                          if(check)write(nchann,106)points_array(npq-1)
     *                          ,points_array(npq)
             enddo
             call grf_points(0,np,npq,points_array
     *                      ,pcolour(nc),lcolour(nb))
      endif
c
      return
c
100   format(/)
101   format(300(/,4i6))
102   format('KML_PREP: NP',i6,', NPOLYGON',i3,', NPAIR',i6)
103   format(200i8)
104   format('#Polygon',i3,' depth',i3)
105   format(i6,2f10.2)
106   format(f0.6,',',f0.6,',',i0)
107   format('depth',i3.3)
109   format(1000(/,f12.5))
110   format('KML_PREP: ndepth,np,k,npair,xpoint(k),ypoint(k)'
     *      ,4i6,2f20.5)
111   format('KML_PREP:',11x,' Left:',2f15.7,', Right:',2f15.7
     *      ,', Depth',2i4)
1111  format(/20x,' Left:',2f15.7,', Right:',2f15.7
     *      ,', Depth',i4,' TOP OF POLYGON',i4)
1112  format(i6,' lines         Left:',2f15.7,', Right:',2f15.7
     *      ,', Depth',i4,' END OF POLYGON',i4,/)
113   format('KML_PREP - verts: NX',i4,' x(nx):',f7.1,' y(nx):',f7.1
     *                       ,' NQ',i4,' x(nq):',f7.1,' y(nq):',f7.1)
114   format('NCOL:',i4,', Blue:',i4,', Green:',i4,', Red:',i4)
116   format(1000(/,f0.6,',',f0.6))
117   format(2(/,7f10.2))
119   format('depth',i3.3,'Polygon',i3.3)
120   format('KML_PREP: Duplicate NQ',i6,2f10.2)
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
1004  format('    <Style id="',a8,'">'
     *     ,/'      <LineStyle>'
     *     ,/'        <width>0</width>'
     *     ,/'      </LineStyle>'
     *     ,/'      <PolyStyle>'
     *     ,/'        <color>B0',3z2.2,'</color>'
     *     ,/'      </PolyStyle>'
     *     ,/'    </Style>'
     *      )
2001  format('//'
     *     ,/'//   Start of a polygon section'
     *     ,/'//'
     *     ,/'     var ',a,' = ['
     *      )
2002  format('new google.maps.LatLng(',f0.6,',',f0.6,'),')
2003  format('                         ];'
     *     ,/' var polygon',i3.3,' = new google.maps.Polygon'
     *     ,/'({paths: ',a,','
     *     ,/'  strokeColor: "#FFFFFF",'
     *     ,/'  strokeOpacity: 0.0,'
     *     ,/'  strokeWeight: 1,'
     *     ,/'  fillColor: "#',3z2.2,'",'
     *     ,/'  fillOpacity: 0.35'
     *     ,/'});'
     *     ,/'polygon',i3.3,'.setMap(map);'
     *     ,/' //'
     *     ,/' //   End of a polygon section'
     *     ,/' //'
     *      )
c
      end
