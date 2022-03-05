      real (8) function slope(x1,y1,x2,y2)
c
c      Returns slope between these two points
c
c     implicit real(8) (a-h,o-z)
      real(4)x1,y1,x2,y2
c
      if(x1.ne.x2
     *  )then
             slope=(y1-y2)/(x1-x2)
         else
             slope=huge(slope)
      endif
c
      end