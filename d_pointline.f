      real(8) function d_pointline(x,y,segment)
c
c      Returns the distance from point (x,y) to line defined by segment
c
      implicit real(8) (a-h,o-z)
c
      real(8),dimension(4) :: segment   !   x1,y1,x2,y2
c
c     write(*,100)x,y,segment
c
      if(segment(1).eq.segment(3)   !  Vertical line
     *  )then
             d_pointline=x-segment(1)   
             return
      endif
c
      if(segment(2).eq.segment(4)   ! Horizontal line
     *  )then
             d_pointline=y-segment(2)   
             return
      endif
c
      s=slope(segment(1),segment(2),segment(3),segment(4))
      d_pointline=(y-(s*x)
     *              -cept(segment(1),segment(2),segment(3),segment(4))
     *            )/sqrt(1+s**2)
c
      return
100   format(/'D_POINTLINE:',6f12.6)
      end
