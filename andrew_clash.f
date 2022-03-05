      logical function andrew_clash(xa1,ya1,xa2,ya2,xb1,yb1,xb2,yb2)
c     
c      Do the lines (xa1,ya1,xa2,ya2) and (xb1,yb1,xb2,yb2) intersect?
c
      implicit real(8) (a-h,o-z)
c
      andrew_clash=.false.
c
      xahi=amax1(xa1,xa2)
      xalo=amin1(xa1,xa2)
      yahi=amax1(ya1,ya2)
      yalo=amin1(ya1,ya2)
c
      xbhi=amax1(xb1,xb2)
      xblo=amin1(xb1,xb2)
      ybhi=amax1(yb1,yb2)
      yblo=amin1(yb1,yb2)
c
      if(xahi.lt.xblo)return
      if(xalo.gt.xbhi)return
      if(yahi.lt.yblo)return
      if(yalo.gt.ybhi)return
c
      dxa=xa1-xa2
      if(dxa.eq.0.0
     *  )then
          dxa=0.0000000001
      endif
      slopea=(ya1-ya2)/dxa
       cepta=((xa1*ya2)-(xa2*ya1))/dxa
c     write(*,100)xa1,ya1,xa2,ya2,slopea,cepta
c
      dxb=xb1-xb2
      if(dxb.eq.0.0
     *  )then
          dxb=0.0000000002
      endif
      slopeb=(yb1-yb2)/dxb
       ceptb=((xb1*yb2)-(xb2*yb1))/dxb
c     write(*,100)xb1,yb1,xb2,yb2,slopeb,ceptb
c
      xintercept=(ceptb-cepta)/(slopea-slopeb)
c     write(*,*)xintercept
c
      if(xintercept.lt.xahi.and.xintercept.gt.xalo.and.
     *   xintercept.lt.xbhi.and.xintercept.gt.xblo
     *  )then
             andrew_clash=.true.
             return
       endif
c
100   format('CLASH:',6f8.3)
c
       end
