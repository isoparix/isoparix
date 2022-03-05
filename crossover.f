      subroutine crossover(slope12,cept12,slope34,cept34
     *               ,s12,s34,x_intercept,y_intercept
     *               ,intersector)
c
c      Do the lines P1-P2 and P3-P4 intersect between P1 and P2 (and
c      between P3 and P4)?
c
c     y=( cept(x3,y3,x4,y4)- cept(x1,y1,x2,y2))
c    * /(slope(x1,y1,x2,y2)-slope(x3,y3,x4,y4))
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: s12,s34
      logical intersector
c
      if(slope12.eq.slope34
     *  )then
             intersector=.false.
             x_intercept=-999.999
             y_intercept=-999.999
             return
       endif
c
      if(slope12.eq.huge(x)
     *  )then
             x_intercept= s12(1)
             y_intercept=(s12(1)*slope34)+cept34
             if((y_intercept-s12(2))*(y_intercept-s12(4)).le.0.
     *          .and.
     *          (y_intercept-s34(2))*(y_intercept-s34(4)).le.0.
     *         )then
                    intersector=.true.
                else
                    intersector=.false.
             endif
             return
      endif
c
      if(slope34.eq.huge(x)
     *  )then
             x_intercept= s34(1)
             y_intercept=(s34(1)*slope12)+cept12
             if((y_intercept-s12(2))*(y_intercept-s12(4)).le.0.
     *          .and.
     *          (y_intercept-s34(2))*(y_intercept-s34(4)).le.0.
     *         )then
                    intersector=.true.
                else
                    intersector=.false.
             endif
             return
      endif
c
  3   continue
c
      if(slope12.eq.0.
     *  )then
             x_intercept=(s12(2)-cept34)/slope34
             y_intercept= s12(2)
             if((x_intercept-s12(1))*(x_intercept-s12(3)).le.0.
     *          .and.
     *          (x_intercept-s34(1))*(x_intercept-s34(3)).le.0.
     *         )then
                    intersector=.true.
                else
                    intersector=.false.
              endif
              return
      endif
c
      if(slope34.eq.0.
     *  )then
             x_intercept=(s34(2)-cept12)/slope12
             y_intercept= s34(2)
             if((x_intercept-s12(1))*(x_intercept-s12(3)).le.0.
     *          .and.
     *          (x_intercept-s34(1))*(x_intercept-s34(3)).le.0.
     *         )then
                    intersector=.true.
                else
                    intersector=.false.
              endif
              return
      endif
c
      y_intercept=((cept12*slope34)-(cept34*slope12))
     *            /(slope34-slope12)
      x_intercept=(y_intercept-cept34)/slope34
c
      if(
     *   (((x_intercept-s12(1))*(x_intercept-s12(3)).le.0.) .and.
     *    ((x_intercept-s34(1))*(x_intercept-s34(3)).le.0.))
     *    .and.
     *   (((y_intercept-s12(2))*(y_intercept-s12(4)).le.0.).and.
     *    ((y_intercept-s34(2))*(y_intercept-s34(4)).le.0.))
     *  )then
             intersector=.true.
         else
             intersector=.false.
      endif
c
      return
c
100   format(/'CROSSOVER:',5f10.5
     *      ,/'          ',5f10.5)
c
      end
      
      
