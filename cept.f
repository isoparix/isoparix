      real (8) function cept(x1,y1,x2,y2)
c
c      Returns intercept of line between these two points
c
      real(4)x1,y1,x2,y2
c
      if(x1.ne.x2
     *  )then
             cept=((x1*y2)-(y1*x2))/(x1-x2)
         else
             cept=huge(cept)
      endif
      write(0,100)x1,y1,x2,y2,cept
c
100   FORMAT('CEPT:',25x,5e14.5)
c
      end
