      real (8) function pyth(xa,ya,za,xb,yb,zb)
c
c      Returns square of Pythagorean distance between A and B
c
      implicit real(8) (a-h,o-z)
c
      pyth=((xa-xb)**2)+((ya-yb)**2)+((za-zb)**2)     
c
      end
