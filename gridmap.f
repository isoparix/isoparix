      subroutine gridmap
c
c      Study the grid and end up with a list of where particular grid
c      values (the 'depth') (x,y)start and (x,y)end in each row of the grid.   
c      The 'number of run-line elements', nplist, is the size of the 
c      collection of all of these elements.
c
      use venn_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical found_segment,new_poly,singleton,polytest,polyactive
     *       ,unsorted,unmatched,bracketed
c
      character(1),dimension(0:1023,200) :: colourchart
      character(1) :: ssq
      character(50) data_file
      character(30) graph_name
      character(45)outcode
c
      integer(4),dimension(-1:16,0:8000) :: depth_count
      integer(4),dimension(     8000) :: tl,tr
c
      real(8),dimension(0:8000) :: tg
c
      integer(4)tzc,tyfinal,trfinal,tlfinal
c
c
c
c      Write Google headers
c
      write(outcode,1181)trim(title)  ! Start of KML Google Earth file
      open(2,file=outcode,form='formatted',status='unknown')
      write(2,1000)
c
      nzoom=12
      write(outcode,1182)trim(title)  ! Start of KML Google Maps file
      open(4,file=outcode,form='formatted',status='unknown')
      write(4,2000)trim(title),ctrlat,ctrlon,nzoom
c
      if(check.or.graph
     *  )then
             graph_name=trim(title)
             al=nxlow
             ah=nxhi
             bl=nylow
             bh=nyhi
             call grf_header(al,ah,bl,bh,graph_name)
      endif
c
      maxdepth=0
      ntplist=0
c
      do iyz=nylow,nyhi
         if(zcount(iyz).gt.0
     *     )then
c
                if(check
     *            )then
                       write(18,220)
     *                 iyz,(zal(mx,iyz),agrid(mx,iyz),zar(mx,iyz)
     *                     ,zbl(mx,iyz),bgrid(mx,iyz),zbr(mx,iyz)
     *                     ,mx=1,zcount(iyz))
                endif
c
                nticks=0
                do nt=1,zcount(iyz)
                   nticks=nticks+1
                   tg(nticks)=agrid(nt,iyz)
                   nticks=nticks+1
                   tg(nticks)=bgrid(nt,iyz)
                enddo  ! 1,zcount(iyz)
c
c      Bubble sort array tg
c
c        write(18,*)nylow,iyz,nyhi,zcount(iyz)
                unsorted=.true.
                do while(unsorted)
                   nx=0
                   do ns=1,nticks-1
                      if(tg(ns).gt.tg(ns+1)
     *                  )then
                             t=tg(ns)
                             tg(ns)=tg(ns+1)
                             tg(ns+1)=t
                             nx=nx+1
                      endif
                   enddo !  ns=1,nticks-1
                   if(nx.eq.0)unsorted=.false.
                enddo ! while
c
                if(check
     *            )then
                       write(18,2201)
     *                 iyz,(tg(mx),mx=1,nticks)
                endif
c
c      Remove duplicates
c
                nx=1
                do ns=2,nticks
                   if(tg(ns).ne.tg(nx)
     *               )then
                          nx=nx+1
                          tg(nx)=tg(ns)
                   endif
                enddo  !  2,nticks
                nticks=nx
c
c
c      Define probe points as the mid-way points between adjacent
c      elements of tg
c
                do ns=1,nticks
                   tl(ns)=-1
                   tr(ns)=-1
                enddo  ! 1,nticks
c
                do ns=1,nticks-1
                   probe=.5*(tg(ns)+tg(ns+1))  !   Probe at mid-way point
c
                   do mx=-1,maxlayer
                      depth_count(mx,ns)=0
                   enddo
c
                   do ic=1,zcount(iyz)
                      if(probe.gt.agrid(ic,iyz).and.
     *                   probe.lt.bgrid(ic,iyz)
     *                  )then   !  Probe lies between an agrid/bgrid pair...
                             depth_count(zar(ic,iyz),ns)
     *                      =depth_count(zar(ic,iyz),ns)+1
                      endif
                   enddo  !  1,zcount(iyz)
                   kburns=depth_count(1,ns)
c
                   if(depth_count(0,ns).eq.0
     *               )then   !  No exclusion zone here
                          ndcount=0
                          do mx=2,maxlayer
                             if(depth_count(mx,ns).gt.0
     *                         )then  !  Probe has hit a real vlue
                                    ndcount=ndcount+1
                                    if(ndcount.gt.maxdepth
     *                                )then
                                           maxdepth=ndcount
                                    endif
                             endif
                          enddo  !  mx=1,maxlayer
                          if(ndcount.gt.0.and.
     *                       kburns.ge.nburns
     *                      )then
                                 tr(ns  )=ndcount
                                 tl(ns+1)=ndcount
                          endif
                      else
                          tr(ns  )=0
                          tl(ns+1)=0
                   endif  ! depth_count(0,ns).eq.0
                   write(18,2204)iyz,kburns,ndcount,probe
     *                          ,nticks,zcount(iyz)
                enddo
c
                if(check.and.zcount(iyz).gt.0
     *            )then
                       write(18,2203)
     *                 iyz,(tl(mx),tg(mx),tr(mx)
     *                 ,mx=1,zcount(iyz))
                endif
c
c      Gather all the edges into one line
c
                nt=0
                do ic=1,nticks
                   if(tl(ic).ne.tr(ic)
     *               )then  ! This is an edge
                          nt=nt+1
                          zg(nt,iyz)=tg(ic)
                          zl(nt,iyz)=tl(ic)
                          zr(nt,iyz)=tr(ic)
                   endif
                   nd=max0(tl(ic),tr(ic))
                enddo
                zcount(iyz)=nt
                if(check.and.zcount(iyz).gt.0
     *            )then
                       write(18,2202) ! Gathered
     *                 iyz,(zl(mx,iyz),zg(mx,iyz),zr(mx,iyz)
     *                 ,mx=1,zcount(iyz))
                endif
                endif ! zcount(iyz) > 0?
      enddo ! iyz=nylow,nyhi
c
c *****************************************************************
c *   We now have all the co-ordinates of where all these circles *
c *   cut this line...                                            *
c *****************************************************************
c
      grid=0
      do iyb=nylow,nyhi
         yg(iyb)=iyb   !   The y-label for this row - subject to change
c
c      Create a grid for a bitmap
c
         do ix=1,zcount(iyb)-1
            if(zr(ix,iyb).eq.zl(ix+1,iyb).and.
     *         zr(ix,iyb).gt.0 !  Avoid exclusion zones
     *        )then ! Next line does the bitmap array
                   grid(int(zg(ix,iyb)):int(zg(ix+1,iyb)),iyb)
     *                     =zr(ix,iyb)
            endif
         enddo  ! ix=1,zcount(iyb)-1
      enddo ! iyb=nylow,nyhi
c
      ncolstep=int(254./float(maxdepth))
      if(check.or.bitmap
     *  )then
             bmname=trim(title)
             call gridbmp
c
             do n=0,255
                m=n*4
                colourchart(m:m+3,:)=char(n)
             enddo
             bmname='colourchart'
cPROB            call array2bmp(1024,200,colourchart)
      endif
c
      nytop=nyhi
c     go to 80
      iyb=nylow-1
      do iyline=nylow,nyhi
         iyb=iyb+1
         bracketed=.false.
c
         if(iyb.gt.nylow      .and.
     *      zcount(iyb-1).gt.0.and.
     *      zcount(iyb  ).gt.0.and.
     *      zcount(iyb-1).ne.zcount(iyb)
     *     )then
                if(zcount(iyb-1).gt.zcount(iyb)
     *            )then
                       iyp=iyb-1  ! 'P' for positive, greater than,...
                       iyq=iyb
                       write(18,122)iyb,iyp,zcount(iyp),iyq,zcount(iyq)
     *                             ,yg(iyp)
                   else
                       iyp=iyb    ! 'P' for positive, greater than,...
                       iyq=iyb-1
                       write(18,121)iyb,iyp,zcount(iyp),iyq,zcount(iyq)
     *                             ,yg(iyp)
                endif  ! zcounts not equal
                tzc=zcount(iyp)       !   The one with the biggest zcount
c
         do my=1,tzc    !   Copy line
            tl(my)=zl(my,iyp)
            tg(my)=zg(my,iyp)
            tr(my)=zr(my,iyp)
         enddo
c
         iz=zr(1,iyp)   !  Colour of this line segment (1:2,iyp)
         za=zg(1,iyp)
         do ix=2,zcount(iyp)
            zb=zg(ix,iyp)
            if(iz.eq.zl(ix,iyp)
     *        )then
                   do mx=2,zcount(iyq)    !  DEBUG Move 't's to 'e's..
                      if(za.ge.zg(mx-1,iyq).and.
     *                   zb.ge.zg(mx-1,iyq).and.
     *                   za.le.zg(mx,iyq).and.
     *                   zb.le.zg(mx,iyq).and.
     *                   iz.ne.zl(mx,iyq)
     *                  )then
                             bracketed=.true.
                             if(check
     *                         )then
                                    nposp=0
                                    do mq=1,ix-1
                                       if(zr(mq,iyp).eq.iz)nposp=nposp+1
                                    enddo
                                    nposq=0
                                    do mq=1,mx-1
                            if(zr(mq,iyq).eq.zr(mx-1,iyq))nposq=nposq+1
                                    enddo
                                    write(18,223)iyp,yg(iyp),za,zb,iz
     *                       ,nposp,zg(mx-1,iyq),zg(mx,iyq),zr(mx-1,iyq)
     *                       ,nposq,yg(iyq)
                             endif
c
c      Reflect the bracketing...
c
c                          za   zb
c            ix-2         ix-1  ix             ix+1
c      iyp     *------------*||||*--------------*   zcount(iyp)>zcount(iyq)
c      NEW     *------------*----*--------------*   New line segment has
c      iyq            *-----------------------*     same value as bracket
c                  zg(mx-1)                 zg(mx)  
c
                             ty=yg(iyp)
                             tr(ix-1)=zr(mx-1,iyq)
                             tl(ix  )=zr(mx-1,iyq)
                      endif
                   enddo
            endif
            za=zb
            iz=zr(ix,iyp)
         enddo
c
         if(bracketed
     *     )then
c
                write(18,2205)yg(iyp)  ! Current line
     *         ,(zl(my,iyp),zg(my,iyp),zr(my,iyp),my=1,zcount(iyp))
                write(18,2205)yg(iyq)  ! Current line
     *         ,(zl(my,iyq),zg(my,iyq),zr(my,iyq),my=1,zcount(iyq))
                write(18,2206)tyfinal,(tl(my),tg(my),tr(my),my=1,tzc)! Altered line
                iys=iyb
                nytop=nytop+1
                do n=nytop,iys+1,-1   !  Push all entries up the stack
                   do m=1,zcount(n-1)
                      zl(m,n)=zl(m,n-1)
                      zg(m,n)=zg(m,n-1)
                      zr(m,n)=zr(m,n-1)
                   enddo
                       yg(n)=yg(n-1)
                   zcount(n)=zcount(n-1)
                enddo
                m=0
                do n=1,tzc
                   if(tl(n).ne.tr(n)
     *               )then
                          m=m+1
                          zl(m,iys)=tl(n)
                          zg(m,iys)=tg(n)
                          zr(m,iys)=tr(n)
                   endif
                enddo
                    yg(iys)=ty
                zcount(iys)=m
                iyb=iyb+1   ! WAS 2 !!
                if(check
     *            )then
                       write(18,2191)int(yg(iys))
     *                   ,(zl(n,iys),zg(n,iys),zr(n,iys),n=1,m)
                       write(18,120)
                endif
            endif
         endif  ! iyb.gt.nylow, zcount(iyb-1).gt.0, zcount(iyb).gt.0
      enddo  ! nylow to nyhi
c
 80   continue
c
      if(check
     *  )then
             do n=nylow,nytop
                write(18,219)n,yg(n)  !  GM line
     *                     ,(zl(mx,n),zg(mx,n),zr(mx,n)
     *                      ,mx=1,zcount(n))
             enddo
      endif
c
      npolygon=0
      do ndepth=1,maxdepth
         nx=1
         do while (nx.gt.0)
            nplist=0
            nx=0
            new_poly=.false.
            polyactive=.true.
            cp=0.0   !  Dummy initial centre point
            do iyq=nylow,nytop
               if(polyactive
     *           )then
                      if(nplist.gt.0)polyactive=.false.
                      cpdiff=10000000.  ! Centre-point difference
                      found_segment=.false.
                      do ix=1,zcount(iyq)
                         if(!zr(ix,iyq).eq.zl(ix+1,iyq).and.
     *                      zr(ix,iyq).eq.ndepth   !  Match depth
     *                     )then
                                if(nplist.gt.0
     *                            )then
                                       new_poly=polytest(
     *                                     xleft (nplist),zg(ix,  iyq)
     *                                    ,xright(nplist),zg(ix+1,iyq)
     *                                                  )
                                 endif
c
                                 if(.not.new_poly
     *                             )then ! This is a candidate
                                        found_segment=.true.
                                        cpn=.5*(zg(ix+1,iyq)+zg(ix,iyq))
                                        cpd=(cpn-cp)**2
                                        if(cpd.lt.cpdiff
     *                                    )then
                                               ixa=ix
                                               cp=cpn
                                               cpdiff=cpd
                                        endif
                                 endif
                         endif !  General conditions on zone
                      enddo  ! IX...
c
                      if(found_segment
     *                  )then
                             nplist=nplist+1
                             xleft (nplist)=zg(ixa,  iyq)
                             xright(nplist)=zg(ixa+1,iyq)
                             yleft (nplist)=yg(iyq)
                             yright(nplist)=yg(iyq)
                             depth (nplist)=ndepth
                             zr(ixa,  iyq)=0
                             zl(ixa+1,iyq)=0
                             nx=nx+1
                             polyactive=.true.
c
                                       if(check
     *                         )then
                                    write(18,222)nplist
     *                               ,int(yleft (nplist))
     *                                   ,xleft (nplist)
     *                                   ,xright(nplist)
     *                                   ,ndepth
                             endif
                      endif  ! found_segment
c
               endif  ! polyactive
            enddo  ! IYQ
            if(nplist.gt.1
     *        )then
                   npolygon=npolygon+1
                   write(18,104)npolygon,ndepth,nplist
                   call kml_prep(nplist,npolygon,ndepth)
            endif
         enddo   !  WHILE...
      enddo   !  NDEPTH...
c
c     if(nplist.le.0
c    *  )then
c            write(0,118)nplist
c            stop
c     endif
c
c      We now have all start and end points for all instances of
c      all depths on all lines
c
c     stop    !  ********** STOP ************
c
c      Write Google trailers
c
      if(check.or.graph)call grf_trailer(npolygon)
c
      write(2,1003)
      write(4,2004)
c
      return
c
100   format('SCAN: nplist,depth,xs,ys,xe,ye',2i7,2(f14.7,f8.1))
1011  format('GRIDMAP POLYGON_END:        ',2i7,2(f14.7,f8.1),/)
1012  format('GRIDMAP POLYGON START:      ',2i7,2(f14.7,f8.1))
1013  format('GRIDMAP: New polygon')
1001  format('POLY_END  ',2i6,23x,2i6,12x,4f15.7,//)
1002  format('POLY_START',2i6,23x,2i6,12x,4f15.7,//)
102   format('POLY',i4,' n  nplist depth',19x,'xleft',19x,'ystart',20x
     *      , 'xright',22x,'yend')
103   format('GRIDMAP: Count is:',i8,' of',i8,' elements.'
     *       ,i9,' found at depth',i3,'.  Starts/ends:',2i6)
104   format('GRIDMAP: npolygon',i3,' depth',i3,', count',i8)
107   format('GRIDMAP ERROR (A): line_start(iy),    ix,iy,ax-/+2'
     *      ,i10,2i5,5f12.5
     *      ,/44x,'bx-/+2',20x,5f12.5
     *      ,/44x,'depth ',16x,5i12)
108   format('GRIDMAP ERROR (B): line_start(iy),ix,iy,ax-/+2'
     *      ,3i5,5f12.5
     *      ,/44x,'bx-/+2',20x,5f12.5
     *      ,/44x,'depth ',16x,5i12)
109   format('GRIDMAP: NPLIST is:',i8,' (was',i8,') of',i8,' elements.'
     *      ,' Delta=',i6)
110   format('GRIDMAP: Single square - ix,iy,grid,agrid,bgrid:'
     *      ,3i5,2f15.7)
111   format('GRIDMAP: End of joining scan')
112   format('Post-chain key:',i5,2(' Runline',i5,2i12,i6,2i5))
1121  format(' Pre-chain key:',i5,2(' Runline',i5,2i12,i6,2i5),/)
113   format(/'GRIDMAP: Start of new polygon',i3)
114   format('GRIDMAP Using nkey',i3,': Sub-chain of runlines from'
     *      ,i5,' to',i5,' in polygon',i3,'.  Sub-chain length=',i5
     *      ,'.  Y-values (x10)',i6,' to',i6)
115   format('GRIDMAP: Polygons reduced from',i4,' to',i4)
116   format('GRIDMAP:  Data pairs',i6,' of',i6
     *      ,': Npolygon',i4,', ndepth',i4,'. Last key=',i4)
117   format('GRIDMAP ERROR: Duplicate at',i5,' and',i5,':',6i12)
118   format('GRIDMAP ERROR: NRUNLINES=',i5)
119   format('Runline',i5' Y=',i5,':'
     *    ,2('  Actual/predicted:',3f7.2,2f7.1))
1191  format('Runline',i5' Y=',i5,':  Start',f8.2
     *                             ,' End=',f8.2,' Depth=',i2)
120   format(/)
121   format('iyb=',i5
     *    ,', iyp=',i5,', zcount(iyp)=',i5
     *    ,', iyq=',i5,', zcount(iyq)=',i5,' IYB <==> IYP',i5,':')
122   format('iyb=',i5
     *    ,', iyp=',i5,', zcount(iyp)=',i5
     *    ,', iyq=',i5,', zcount(iyq)=',i5,' IYB <==> IYQ',i5,':')
123   format('Line=',i3,' Y=',i5,' ndepth=',i3
     *        ,': Current/old slots=',2i3
     *       ,' Current/old zcounts=',2i3
     * ,' Differences zcounts/slots=',2i3)
204   format(/'GRIDMAP: Last element')
205   format(/'GRIDMAP:   Seeking',i4,4f15.7)
206   format( 'GRIDMAP:     Found',i4,4f15.7,', N=',i6,/)
207   format( 'GRIDMAP: Not found',i4,3f15.7,/)
219   format('GM line',i5,' Y=',i5,':',100(i3,f7.2,i3))
2191  format( 'Extra new at Y=',i5,':',100(i3,f7.2,i3))
220   format(/'Ticks        Y=',i5,':',100(i3,f7.2,i3))
2201  format( 'Sorted       Y=',i5,':',100(  f10.2,3x))
2202  format( 'Gathered     Y=',i5,':',100(i3,f7.2,i3))
2203  format( 'Probed       Y=',i5,':',100(i3,f7.2,i3))
2204  format( 'Sample       Y=',i5,': Burns=',i3,100(i3,f7.2,i3))
2205  format( 'Current line Y=',i5,':',100(i3,f7.2,i3))
2206  format( 'Altered line Y=',i5,':',100(i3,f7.2,i3))
221   format(1000i6)
222   format('Runline',i5,' Y=',i5,':',2f7.2,i3)
223   format('Continuation at line',i5,', Y-value',i5
     *      ,' between',2f10.2
     *      ,' (level',i2,' pos',i2') bracketed by',2f10.2
     *      ,' (level',i2,' pos',i2') at Y=',i5)
224   format('ERROR in GRIDMAP: Right at tick',i2,' (',i2,')'
     *                        ,' Left at tick',i2,' (',i2,')'
     *      ,' not equal at line',i4)
1000  format(' <kml xmlns="http://www.opengis.net/kml/2.2">'
     *     ,/'  <Document>')
1003  format('   </Document>'
     *     ,/'</kml>'
     *      )
1181  format(a,'_poly_earth.kml')
1182  format(a,'_poly_maps.html')
2000   format('<!DOCTYPE html>'
     *      ,/'<html>'
     *      ,/'  <head>'
     *      ,/'  <meta name="viewport" content="initial-scale=1.0'
     *      ,                                ',user-scalable=no">'
     *      ,/'  <meta charset="utf-8">'
     *      ,/'  <title>Google Maps JavaScript API v3 ',a,'</title>'
     *      ,/'  <link href="http://code.google.com//apis/maps/'
     *      ,     'documentation/javascript/examples/default.css"'
     *      ,     ' rel="stylesheet" type="text/css" rel="stylesheet">'
     *      ,/'  <script type="text/javascript" '
     *      ,/      'src="https://maps.googleapis.com/maps/api/js?'
     *      ,       'key=AIzaSyBrt_zwpr1_uZXfGENhPDVQCIuNZIVKPk8'
     *      ,       '&sensor=false">'
     *      ,/'  </script>'
     *      ,/'  <script type="text/javascript">'
     *      ,/'    var map;'
     *      ,/'    var infoWindow;'
     *      ,/'    function initialize() {'
     *      ,/'    var myCentre = new google.maps.LatLng'
     *      ,                       '(',f0.6,',',f0.6');'
     *      ,/'    var mapOptions = {zoom: ',i3,', center: myCentre'
     *      ,       ',mapTypeId: google.maps.MapTypeId.ROADMAP};'
     *      ,/'      map = new google.maps.Map(document.getElementById'
     *      ,       "('map-canvas'),mapOptions);"
     *       )
2004  format(' }'
     *     ,/'</script>'
     *     ,/'   </head>'
     *     ,/'   <body onload="initialize()">'
     *     ,/'     <div id="map-canvas"></div>'
     *     ,/'   </body>'
     *     ,/'</html>'
     *      )
c
      end
