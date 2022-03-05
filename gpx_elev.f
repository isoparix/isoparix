      program gpx_elev
c
c      Reads GPX file, creates graph of time-of-day vs elevation
c
      implicit real(8) (a-h,o-z)
c
      character (1) gps_text(120),ele(5),tod(8)
      character (120)gps_input
      character (5) eletext
      character (8) todtext
      equivalence (eletext,ele),(gps_input,gps_text),(tod,todtext)
c
      real(8),dimension(10000) :: wallclock,elevation
      real(8),allocatable,dimension (:) :: points_array
c
      character(8)fillcol,linecol
      character(30)graph_name,series_name
c
      xmin= 1000000.
      xmax=-1000000.
      ymin= 1000000.
      ymax=-1000000.
c
      nline=0
   1  continue
      read(7,100,end=2)gps_input
      i=0
      j=0
      do n=1,115
         ele=gps_text(n:n+4)
         if(eletext.eq.'<ele>'
     *     )then
                i=n+5
                do m=i,i+8
                   ele=gps_text(m:m+4)
                   if(eletext.eq.'</ele'
     *               )then
                          j=m-1
                          nline=nline+1
                    endif
                enddo
          endif
c
c      Now read the time of day
c
         if(gps_text(n).eq.'T'.and.i*j.gt.0
     *     )then
c      
                 ele=gps_text(j-4:j)
                 ele(1:5-(j-i+1))=' '
c
                 tod=gps_text(n+1:n+8)
                 read(todtext,102,err=99)ihrs,mins,isecs
                 itod=(ihrs*3600)+(mins*60)+isecs
c
                 wallclock(nline)=itod
                 read(eletext,*)elevation(nline)
                 write(2,101)itod,eletext
                 if(nline.gt.navg
     *             )then
                        write(4,103) ! wallclock(nline),elevation(nline)
     *                           ravg*sum(wallclock(nline-navg:nline))
     *                          ,ravg*sum(elevation(nline-navg:nline))
                 endif
c
                 exit
         endif
       enddo
                 
       go to 1
    2  continue
c
       lines_read=nline ! For reference
c
       xmax=maxval(wallclock(1:nline))
       xmin=minval(wallclock(1:nline))
       ymax=maxval(elevation(1:nline))
       ymin=minval(elevation(1:nline))
c
       write(*,104)
       read(*,*)navg
      nsavg=navg/2
      ravg=1./float(navg)
      npoints=nline/navg

       allocate(points_array(2*npoints))
       m=0
       do n=1,npoints
          mx2=n*navg
          mx1=mx2-navg+1
          m=m+1
          points_array(m)=ravg*sum(wallclock(mx1:mx2))
          m=m+1
          points_array(m)=ravg*sum(elevation(mx1:mx2))
       enddo
c
c      Now create a grf file
c
      graph_name='GPX Time vs Elevation'
      call grf_header(xmin,xmax,ymin,ymax,graph_name)
c
      series_name='Elevation'
      fillcol='Red     '
      linecol='Blue    '
      call grf_points(0,1,2*npoints,points_array
     *               ,fillcol,linecol,2,1,series_name)
      call grf_trailer(1)
c
       stop
 99    continue
      write(0,*)todtext
      stop
100   format(a120)
101   format(i6,',',a5)
102   format(i2,x,i2,x,i2)
103   format(f8.2,',',f8.2)
104   format('Enter the number of readings to centre-average')
       end
