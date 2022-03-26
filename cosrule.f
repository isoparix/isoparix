      real (8) function cosrule(ax,ay,bx,by,cx,cy)
c
c      Takes in xy coordinates of triangle ABC.  
c      Returns cos B
c
      implicit real(8) (a-h,o-z)
c
      a2=((bx-cx)**2)+((by-cy)**2)
      b2=((ax-cx)**2)+((ay-cy)**2)
      c2=((ax-bx)**2)+((ay-by)**2)
      denom=2.*sqrt(a2*c2)
      if(denom.le.0.D0   ! Can't be < 0 (squares), but you never know...
     *  )then
             cosrule=0.D0
         else
             cosrule=(a2+c2-b2)/denom
      endif
c     write(*,100)cosrule,ax,ay,bx,by,cx,cy
c
100   format('COSRULE=',f10.7,' from vertices',6(/f25.3))
      end
