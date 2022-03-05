      subroutine grf_header(xmin,xmax,ymin,ymax,graph_name)
c
      implicit real(8) (a-h,o-z)
c
      character(30) graph_name
c
      open(20,file=trim(graph_name)//'.grf',status='unknown')
c
      span_margin=0.05*(xmax-xmin)
      x_axis_min=xmin-span_margin
      x_axis_max=xmax+span_margin
c
      span_margin=0.05*(ymax-ymin)
      y_axis_min=ymin-span_margin
      y_axis_max=ymax+span_margin
      x_axis_cross=0.0
c     x_axis_cross=(y_axis_min+y_axis_max)/2.0
c
      write(20,100)x_axis_min,x_axis_max,y_axis_min,y_axis_max
     *            ,x_axis_cross,xmin,1
c
      return
c
100   format('[Graph]'
     *     ,/'Version = 4.3.0.384'
     *     ,/'MinVersion = 2.5'
     *     ,/'OS = NT 6.1 Service Pack 1'
     *     ,/
     *     ,/'[Axes]'
     *     ,/'xMin =',e12.5
     *     ,/'xMax =',e12.5
     *     ,/'xShowGrid = 1'
     *     ,/'yMin =',e12.5
     *     ,/'yMax =',e12.5
     *     ,/'yShowGrid = 1'
     *     ,/'xAxisCross =',e12.5
     *     ,/'yAxisCross =',e12.5
     *     ,/'AxesColor = clBlu     *     '
     *     ,/'GridColor = 0x00FF9999'
     *     ,/'ShowLegend = 0'
     *     ,/'Radian =',i2
     *     ,/'AxesStyle = 2'
     *     ,/)
c
      end
