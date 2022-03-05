      subroutine grf_points(nstep,npstanza,ncoord,points_array
     *                     ,fillcol,linecol,nsize,nvisible,series_name)
c
c      points_array is a succession of real-number elements, an x-value,
c      followed by a y-value
c
c      If nstep is zero, then points are joined, as line graph
c      If nstep is not zero, then points are plotted as bar graph
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(   ncoord)    :: points_array
      real(8),dimension((2*ncoord)+1) :: step_array
      character(8) fillcol,linecol
      character(30)series_name
c
c      These are available colour names
c
c      Red
c      Aqua
c      Blue
c      Fuchsia
c      Lime
c      Yellow
c      Black
c
      write(20,1011)npstanza,fillcol,linecol,nsize,nvisible
c
c         Points              Step
c   N   X    N   Y     N   X     N   Y
c                      1  0.5    2  20.0
c   1   1.0  2  20.0   3  1.5    4  20.0
c                      5  1.5    6  30.0
c   3   2.0  4  30.0   7  2.5    8  30.0
c                      9  2.5   10  40.0
c   5   3.0  6  40.0  11  3.5   12  40.0
c
      if(nstep.eq.0
     *  )then
             write(20,1012)points_array
         else
             delta_x=0.5*(points_array(ncoord-1)-points_array(1))
     *                   /float((ncoord/2)-1)
             if(nstep.eq.1   ! In case we think of other options
     *         )then
c                         
                 step_array(1)=points_array(1)-delta_x
                 do n=1,ncoord-2,2  ! All the x-coordinates
                    cx=0.5*(points_array(n+2)-points_array(n))
                    step_array((2*n)+1)=points_array(n)+cx
                    step_array((2*n)+3)=points_array(n)+cx
                 enddo
                 step_array((2*ncoord)-3)=step_array((2*ncoord)-5)
                 step_array((2*ncoord)-1)=points_array(ncoord-1)+delta_x
c                         
                 do n=2,ncoord,2  ! All the y-coordinates
                    step_array( 2*n   )=points_array(n)
                    step_array((2*n)-2)=points_array(n)
                 enddo
c                         
                 write(20,1012)step_array
c                         
             endif
      endif
c
      write(20,1013)trim(series_name)
c
      return
c
1011  format('[PointSeries',i0,']'
     *     ,/'FillColor = cl',a8
     *     ,/'LineColor = cl',a8
     *     ,/'Size =',i2
     *     ,/'Style = 0'
     *     ,/'LineStyle = 0'
     *     ,/'Visible =',i2
     *     ,/'LabelPosition = 1')
1012  format('Points =',4000(f0.16,',',f0.16,';'))
1013  format('LegendText =',a,/)
c
      end
