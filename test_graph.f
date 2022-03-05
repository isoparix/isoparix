      program test_graph
c
c      Tests grf_* routines
c
      implicit real(8) (a-h,o-z)
      dimension points_array(1000)
      character(8)fillcol,linecol
      character(30)graph_name,series_name
c
      ncoord=1
  1   read(*,*,end=2,err=99)points_array(ncoord)
      ncoord=ncoord+1
      go to 1
  2   continue
      ncoord=ncoord-1
c
      xmin= 1000000.
      xmax=-1000000.
      ymin= 1000000.
      ymax=-1000000.
c
      do n=1,ncoord-1,2
         if(points_array(n).gt.xmax)xmax=points_array(n)
         if(points_array(n).lt.xmin)xmin=points_array(n)
      enddo
c
      do n=2,ncoord,2
         if(points_array(n).gt.ymax)ymax=points_array(n)
         if(points_array(n).lt.ymin)ymin=points_array(n)
      enddo
c
      graph_name='test_line_step'
      call grf_header(xmin,xmax,ymin,ymax,graph_name)
c
      series_name='test_line'
      fillcol='Red     '
      linecol='Blue    '
      call grf_points(0,1,ncoord,points_array
     *               ,fillcol,linecol,2,1,series_name)
c
      series_name='test_step'
      fillcol='Lime    '
      linecol='Red     '
      call grf_points(1,2,ncoord,points_array
     *               ,fillcol,linecol,2,1,series_name)
c
      call grf_trailer(2)
c
 99   continue
      stop
      end
