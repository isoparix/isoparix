      program andrew_tt
c
      use andrew_comms
c
      implicit real(8) (a-h,o-z)
      character(8) oc1,oc2,oca,ocb
      character(1) graph
      character(10) units
      character(20) graph_name
      integer andrew_code_index
      integer,dimension(2,3) :: nearindex
      real(8),dimension(2,3) :: neardist,nearspeed,neartt
      real(8),dimension(3,3) :: sdist
      real(8), dimension(2) :: x,y,avg_speed
      real(8), dimension(8) :: xdata,ydata,points
      dimension dist(4000),temp(3),ixmin(2,3)
      logical pair,ocmiss,no_graph
c
c      1. Read in two lon/lat pairs
c      2. Find the nearest three outcodes to each of them
c      3. Check that we know the travel times between the outcodes on
c         each side of the triangle
c      4. Find the mean of these three times for each triangle, which
c         we will assume yields the travel speed within the triangle
c      5. Find the shortest travel time between the lon/lat pairs
c         a) time to travel a near outcode
c         b) time from that outcode to outcode in the distant triangle
c         c) time to travel from distant outcode to distant point
c
c
c      Co-ordinates of individual postcodes
c
c       AL6,-0.210662540048361,51.8308972846717
c       AL7,-0.185060016810894,51.7969649098814
c       AL8,-0.232055252417922,51.8023744225502
c       AL9,-0.195197928696871,51.7460117395967
c
c      Travel times: outcode1, outcode2, hh, mm, ss, miles
c
c       BS1,BS14,00,13,16,4.902618768
c       BS14,BS1,00,13,01,4.809413088
c       BS1,BS15,00,15,59,5.666905344
c       BS15,BS1,00,15,15,5.350006032
c
      call andrew_get_outcode_ll
      call andrew_get_tt
c
      speed=3600*distance/dfloat(tt)
c     radius=6378.1D+00 ! Kilometres
c     units ='kilometres'
      radius=3963.2D+00 ! Miles
      units ='miles     '
      degrad=3.14159265358979D+00/180.0D+00
c
  1   continue
      nseries=0
      write(*,111)
      read(*,*,end=99)x(1),y(1),x(2),y(2),graph
c
      if(graph.eq.'y'
     *  )then
             no_graph=.false.
         else
             no_graph=.true.
      endif
c
c      Find 'flat-earth' coordinates for the graph for these lons/lats
c
      nseries=nseries+1
      call dfd(x(1),y(1),xdata(nseries),ydata(nseries))
      nseries=nseries+1
      call dfd(x(2),y(2),xdata(nseries),ydata(nseries))
c
c      Find distance from the two locations to every outcode
c
c     write(*,101)
      write(*,*)x(1),y(1),x(2),y(2),no_graph
      do location=1,2
         alat=degrad*y(location)
         alon=degrad*x(location)
         dsalat=dsin(alat)
         dcalat=dcos(alat)
         dist=1000000.0
         do j=1,noutrecs
            blat=degrad*outlat(j)
            blon=degrad*outlon(j)
c
c      Returns distance on surface of sphere radius r between
c      lonlatA and lonlatB now in radians.
c
            dist(j)=radius*(dacos(
     *                      (dsalat*dsin(blat))+
     *                      (dcalat*dcos(blat)*dcos(alon-blon))
     *                           )
     *                     )
c
c           write(8,102)'        ',x(location),y(location),
c    *                  outcode(j),outlon(j),outlat(j),dist(j)
         enddo
c
c      Find the nearest three outcodes to the location being looked at
c
         do n=1,3
            k=minloc(dist,1)   !  Index in the outcode list
            nearindex(location,n)=k
            neardist (location,n)=dist(k)
            ixmin(location,n)=k
            temp(n)=dist(k)
c           write(*,102)'        ',x(location),y(location),
c    *                  outcode(k),outlon(k),outlat(k),dist(k)
            dist(k)=1000000.0
            nseries=nseries+1
            call dfd(outlon(k),outlat(k),xdata(nseries),ydata(nseries))
         enddo
c
         do n=1,3
            dist(ixmin(location,n))=temp(n)
         enddo
c
         nsum_tt=0.0
         sum_dist=0.0
         do m=1,3
            n=m+1
            if(n.gt.3)n=1
c           write(*,*)m,n,outcode(ixmin(location,m))
c    *                   ,outcode(ixmin(location,n))
c
            do icx=1,2
               if(icx.eq.1
     *           )then
                      oca=outcode(ixmin(location,m))
                      ocb=outcode(ixmin(location,n))
                  else
                      oca=outcode(ixmin(location,n))
                      ocb=outcode(ixmin(location,m))
               endif
c
               nx=andrew_code_index(oca)
               ocmiss=.true.
               do noc=nx,nttrecs
                  if(ttcodea(noc).eq.oca.and.
     *               ttcodeb(noc).eq.ocb
     *              )then
                         ocmiss=.false.
c                        write(*,103)oca,ocb,tt(noc),distance(noc)
c    *                                              ,speed(noc)
                         nsum_tt  =nsum_tt+tt(noc)
                         sum_dist=sum_dist+distance(noc)
                         exit
                  endif
               enddo
c
               if(ocmiss
     *           )then
                      do lx=1,location
                         write(0,*)(outcode(ixmin(lx,mx)),mx=1,3)
                      enddo
                      write(0,105)oca,ocb
                      stop
               endif
            enddo   ! icx
         enddo   ! m
         avg_speed(location)=3600.*sum_dist/dfloat(nsum_tt)
c        write(*,106)sum_dist,nsum_tt,avg_speed(location)
c
c        write(*,101)
         do nearoc=1,3
            neartt(location,nearoc)=neardist(location,nearoc)/
     *                             avg_speed(location)
c           write(*,107)x(location),y(location)
c    *               ,outcode(nearindex(location,nearoc))
c    *                          ,neartt(location,nearoc)
         enddo
c
      enddo  ! location
c
c      FInd the sphere distances between the distant triangle vertices
c
      do na=1,3
         alat=outlat(ixmin(1,na))
         alon=outlon(ixmin(1,na))
         do ma=1,3
            blat=outlat(ixmin(2,ma))
            blon=outlon(ixmin(2,ma))
            sdist(na,ma)=sphere_dist(alon,alat,blon,blat,radius)
         enddo
      enddo
c
c     write(*,108)(mx,xdata(mx),ydata(mx),mx=1,8)
c
      t=1.d0/3600d0
      ttmin=1000000.0
c
      write(*,110)
      do n=1,3
         oca=outcode(ixmin(1,n))
         nx=andrew_code_index(oca)
         do m=1,3
            ocb=outcode(ixmin(2,m))
            ocmiss=.true.
            do noc=nx,nttrecs
                  if(ttcodea(noc).eq.oca.and.
     *               ttcodeb(noc).eq.ocb
     *              )then
                         alltt=(t*dfloat(tt(noc)))
     *                              +neartt(1,n)+neartt(2,m)
                         write(*,109)oca,ocb
     *                            ,t*dfloat(tt(noc)),neartt(1,n)
     *                            ,neartt(2,m),alltt,distance(noc)
     *                            ,sdist(n,m)
                         if(alltt.lt.ttmin
     *                     )then
                                ttmin=alltt
                                oc1=oca
                                oc2=ocb
                                nocmin=noc
                                noc1=n
                                moc2=m
                         endif
                  endif
            enddo   ! noc
         enddo   ! m
      enddo   ! n
c      
      write(*,101)
      write(*,109)oc1,oc2,t*dfloat(tt(nocmin))
     *           ,neartt(1,noc1),neartt(2,moc2)
     *           ,ttmin,distance(nocmin),sdist(noc1,moc2)
c      
      if(no_graph)go to 1
c
      xmax=maxval(xdata)
      xmin=minval(xdata)
      ymax=maxval(ydata)
      ymin=minval(ydata)
c
      xcen=.5*(xmax+xmin)
      ycen=.5*(ymax+ymin)
      semi_span=1.02*(.5*amax1(xmax-xmin,ymax-ymin))
c
      xmax=xcen+semi_span
      xmin=xcen-semi_span
      ymax=ycen+semi_span
      ymin=ycen-semi_span
c
      graph_name='outcode'
      call grf_header(xmin,xmax,ymin,ymax,graph_name)
c
      points(1)=xdata(3)
      points(2)=ydata(3)
      points(3)=xdata(4)
      points(4)=ydata(4)
      points(5)=xdata(5)
      points(6)=ydata(5)
      points(7)=xdata(3)
      points(8)=ydata(3)
      call grf_points(0,1,8,points,'Red     ','Lime    ')
c
      points(1)=xdata(6)
      points(2)=ydata(6)
      points(3)=xdata(7)
      points(4)=ydata(7)
      points(5)=xdata(8)
      points(6)=ydata(8)
      points(7)=xdata(6)
      points(8)=ydata(6)
      call grf_points(0,2,8,points,'Red     ','Lime    ')
c
      points(1)=xdata(1)
      points(2)=ydata(1)
      points(3)=xdata(2+noc1)
      points(4)=ydata(2+noc1)
      points(5)=xdata(5+moc2)
      points(6)=ydata(5+moc2)
      points(7)=xdata(2)
      points(8)=ydata(2)
      call grf_points(0,3,8,points,'Red     ','Blue    ')
c
      call grf_trailer(3)
c
c
      go to 1
 99   continue
      stop
c
100   format(a10,2i6,2f20.14)
101   format(/)
102   format(2(a10,2f12.7),f10.3)
103   format(2a10,i5,3f8.3)
104   format('Invalid outcode: ',a10)
105   format(2a10,' - outcode pair not found in travel time table')
106   format(/'Total distance:',f8.3,', Total seconds:',i5
     *       ,', Avg speed:',f8.3,/)
107   format('Time from (',f12.7,',',f12.7,') to ',a10,' is',f8.4
     *       ,' hours')
108   format(i4,2f12.3)
109   format(2a10,4f9.4,2f9.2)
110   format(/20x,'    Table   LocalA   LocalB    Total    Table'
     *      ,/20x,'   travel   travel   travel   travel  outcode'
     *      ,     '   Sphere'
     *      ,/'OutcodeA  OutcodeB  ',4('     time'),2(' distance')
     *      ,/) 
111   format(/'Enter lonA, latA, lonB, latB, y/n (graph/no graph)')
200   format(2(4f30.18,/))
c
      end
