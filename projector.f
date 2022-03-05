      real *8 function projector(a1,b1,a2,b2,b0)
c
      implicit real *8 (a-h,o-z)
c
c      Given a1,b1,a2,b2 and b0, return value of a0
c
      projector=a1+((a2-a1)*(b0-b1)/(b2-b1))
c
c     write(*,100)a1,b1,a2,b2,b0,projector
      return
c
100   format('Fixed points a1,b1,a2,b2:',4f10.3
     *      ,'. Value at b=',f10.3,' is a=',f10.3)
c
      end
