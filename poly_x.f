      real(8) function poly_x(x1,y1,x2,y2,y3)
c
c      Takes the points at (x1,y1) and (x2,,y2) and predicts
c      a value for x3 at y3
c
      implicit real(8) (a-h,o-z)
c
      poly_x=(y3-cept(x1,y1,x2,y2))/slope(x1,y1,x2,y2)
c
      return
c
      end
