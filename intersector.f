      logical function intersector(slope12,cept12,slope34,cept34
     *                            ,y1,y2,y3,y4)
c
c      Do the lines P1-P2 and P3-P4 intersect between P1 and P2 (and 
c      between P3 and P4)?
c
c     y=( cept(x3,y3,x4,y4)- cept(x1,y1,x2,y2))
c    * /(slope(x1,y1,x2,y2)-slope(x3,y3,x4,y4))
c
      implicit real(8) (a-h,o-z)
c
      if(slope12.eq.slope34.or.
     *   slope34.eq.123456789.0.or.
     *    cept34.eq.123456789.0.or.
     *   slope12.eq.123456789.0.or.
     *    cept12.eq.123456789.0
     *  )then
c
c      Lines are parallel, or problem unresolvable..!!
c
             intersector=.false.
             return
      endif
c
c     write(0,*)"cept12,slope12,cept34,slope34"
c     write(0,*) cept12,slope12,cept34,slope34
      y=((cept12*slope34)-(cept34*slope12))/(slope34-slope12)
c
      if((y-y1)*(y-y2).lt.0.
     *   .and.
     *   (y-y3)*(y-y4).lt.0.
     *  )then
             intersector=.true.
         else
             intersector=.false.
      endif
c
      end
