      real (8) function slope(x1,y1,x2,y2)
c
c      Returns slope between these two points
c
      implicit real(8) (a-h,o-z)
c
      if(x1.ne.x2
     *  )then
             slope=(y1-y2)/(x1-x2)
         else
             slope=huge(slope)
      endif
c
c     write(0,100)x1,y1,x2,y2,slope
c
100   FORMAT('SLOPE:',24x,5e14.5)
      end
