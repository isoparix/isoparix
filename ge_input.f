      subroutine ge_input
c
c      Read in polygon and cicle co-ordinates
c
c      First two character define type of data
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      logical ge_coords,intersector,intersected,convex,r1set,section
     *       ,lseg_12(400),lseg_21(400),valid,inside,r2set,nolayer
      character(1)  :: space,data_array(10000)
      character(2)  :: data_type
      character(80) :: coordinate 

      character(10000) :: data_txt
      equivalence(data_txt,data_array)
c
      real(8) lon2x,lat2y
      real(8), dimension(4)     :: segment1,segment2
c
      notmade=0
      ignored=0
      ndata=0
      ge_coords=.false.
      r1set=.false.
      r2set=.false.
      nolayer=.true.
      ctrlon=-999.0
      ctrlat=-999.0
      rmax=0.
      rmin=1000000.
      xmin=huge(x)
      xmax=-xmin
      ymin=huge(y)
      ymax=-ymin
      check=.false.   ! No debugging, by defaultt
      bitmap=.false.  ! No bitmaps, by default
      npixels=300000
      extend=.false.
      title='default'
      nburns=0
      maxlayer=-10000
      minlayer= 10000
c
c     Polygon    Polygon
c    descriptor   type
c
c        0       Exclusion (blanks all references to other information)
c        1       Additive, filled, circle
c        2       Closed polygon (has inside and outside)
c        3       Open polygon (no distinction between inside and outside)
c
  1   continue
      read(3,102,end=2)data_type,data_txt
c     write(*,104)data_type,data_txt
c
      if(data_type.eq.'ly'
     *  )then ! Sets current Layer ID
             read(data_txt,*,end=2)layer
c
             if(layer.gt.maxlayer
     *         )then
                    maxlayer=layer
             endif
c
             if(layer.lt.minlayer
     *         )then
                    minlayer=layer
             endif
c
             if(layer.eq.1
     *         )then
                    nburns=nburns+1
             endif
c
             nolayer=.false.
      endif
c
      if(data_type.eq.'gr'
     *  )then !   Turns on graph creation
             graph=.true.
      endif
c
      if(data_type.eq.'st'
     *  )then !   Turns off creation of line segments
             extend=.false.
      endif
c
      if(data_type.eq.'ex'
     *  )then !   Turns on creation of line segments
             extend=.true.
      endif
c
      if(data_type.eq.'bm'
     *  )then !   Turns on bitmap creation
             bitmap=.true.
      endif
c
      if(data_type.eq.'db'
     *  )then !   Turns on debugging information
             check=.true.
      endif
c
      if(data_type.eq.'ti'
     *  )then !   This is the title of the output files
             read(data_txt,*,end=2)title
      endif
c
      if(data_type.eq.'rs'
     *  )then !   This is the pixel count in the map area
             read(data_txt,*,end=2)lpm
             lines_per_mile=lpm
      endif
c
      if(data_type.eq.'pd'
     *  )then !   This is a polygon descriptor - stays until reset
             read(data_txt,*,end=2)npd
      endif
c
      if(data_type.eq.'OR'
     *  )then !   These are the ORigin latitude and latitude
             read(data_txt,*,end=2)ctrlon,ctrlat
             call origin
      endif
c
      if(data_type.eq.'r1'
     *  )then   !  This is a minimum boundary or radius that stays until reset
             r1set=.true.
             read(data_txt,*,end=2)r1
      endif
c
      if(data_type.eq.'r2'
     *  )then   !  This is a maximum boundary or radius that stays until reset
             read(data_txt,*,end=2)r2
             if(r2.gt.rmax)rmax=r2
             r2set=.true.
      endif
c
      if(data_type.eq.'ge'
     *  )then              !   These are longitude and latitude
c
             if(ctrlon.eq.-999.0.or.ctrlat.eq.-999.0
     *         )then
                    write(*,106)
                    stop
             endif
c
             ndata=ndata+1
             ge_coords=.true.
             read(data_txt,*,end=2)px,py
             poly_layer(ndata)=layer
             xpoly(ndata)=lon2x(px)
             ypoly(ndata)=lat2y(py)
             r1poly(ndata)=r1
             r2poly(ndata)=r2
             if(xpoly(ndata)+r2.gt.xmax)xmax=xpoly(ndata)+r2
             if(xpoly(ndata)-r2.lt.xmin)xmin=xpoly(ndata)-r2
             if(ypoly(ndata)+r2.gt.ymax)ymax=ypoly(ndata)+r2
             if(ypoly(ndata)-r2.lt.ymin)ymin=ypoly(ndata)-r2
             poly_type(ndata)=data_type
      endif
c
      if(data_type.eq.'--'.or.data_type.eq.'++'
     *  )then               !  This is the start or end of a group of points
             ndata=ndata+1
             poly_type(ndata)=data_type
      endif
c
      if(data_type.eq.'pg'.or.data_type.eq.'ci'
     *  )then               !  This is general polygon or circle data
             ndata=ndata+1
             poly_layer(ndata)=layer
             read(data_txt,*,end=2)xpoly(ndata),ypoly(ndata)
             r1poly(ndata)=r1
             r2poly(ndata)=r2
             if(xpoly(ndata)+r2.gt.xmax)xmax=xpoly(ndata)+r2
             if(xpoly(ndata)-r2.lt.xmin)xmin=xpoly(ndata)-r2
             if(ypoly(ndata)+r2.gt.ymax)ymax=ypoly(ndata)+r2
             if(ypoly(ndata)-r2.lt.ymin)ymin=ypoly(ndata)-r2
             poly_type(ndata)=data_type
      endif
c
      go to 1
c
  2   continue
c
c      All data read
c
      if(check
     * )then
             write(18,115)
             do n=1,ndata
                write(18,103)n,xpoly(n), ypoly(n)
     *                      ,r1poly(n),r2poly(n)
     *                      ,poly_type(n),poly_layer(n)
             enddo
             call flush(18)
      endif
c
      if(nolayer
     *  )then
             write(0,118)
             stop
      endif
c
      if(.not.r1set.or..not.r2set
     *  )then
             write(0,111)
             stop
      endif
c
c      We now have a collection in Cartesian miles of polygon vertices
c      and individual centres of circles.
c
      miles_per_line=1.0/lines_per_mile  !   Resolution
      if(check)write(*,114)miles_per_line,xmin,xmax,ymin,ymax,ndata
     *                    ,nburns
      call poly_alloc
      call grid_alloc
c
      section=.false.
      do n=1,ndata
c
         if(poly_type(n).eq.'--'
     *     )then
                na=n+1
                section=.true.
         endif
c
         if(poly_type(n).eq.'++'
     *     )then
                if(section
     *            )then
                       nb=n-1
                       section=.false.
                       if(check)write(18,101)na,nb
                       call polyprep(na,nb)
                   else
                       write(*,117)
                       stop
                endif
         endif
c
      enddo
c
c      Extra data created?
c
      return
c
100   format(2f10.2,/2f10.2)
101   format('GE_INPUT: na,nb forwrded to POLYPREP',2i6)
102   format(a2,a80)
103   format("ndata=",i4," xpoly=",f7.2," ypoly=",f7.2," r1poly=",f7.2
     *      ," r2poly=",f7.2," poly_type=",a4," layer=",i3)
104   format('Type: ',a2,', Text: ',a80)
105   format(/)
106   format(/'No central lon lat given.    Must have line like this '
     *     ,  '(eg: for Kingston upon Thames):'
     *     ,//'OR -0.2973 51.41684',/)
107   format('Lon:',2f10.5,', Lat:',2f10.5)
108   format('Segment:',i3,' (x1,y1):',2f10.5,', (x2,y2):',2f10.5
     *      ,', Slope:',f10.5,', Intercept:',f10.5)
109   format('Number of data points:',i4)
110   format('Intercept:',2f12.6,', Displaced:',2f12.6)
111   format(/,'GE_INPUT EROR: Need inner (r1) and outer (r2)',
     *         ' radii to be set',/)
112   format('Inside from:',2f10.5,'?  ',l1)
113   format('Enter resolution, as fraction of boundary eg 8 for 1/8')
114   format('GE_INPUT:miles_per'
     *   ,'_line      xmin      xmax      ymin      ymax  ndata nburns'
     *     ,/'             ',5f10.5,2i7)
115   format('Original data read')
116   format('More data created')
117   format('End of data section (++) encount'
     *       'ered, but no beginning (--)')
118   format(/,'GE_INPUT EROR: Must set layer value with ly tag')
300   format('OK to here...')
c
      end
