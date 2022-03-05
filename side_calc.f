      subroutine side_calc(ixm,iym,nslaves,nx,ny)
c
      i=0.5+sqrt(float(ixm*iym)/float(nslaves))
      n=ixm/i
      if(n.eq.0)n=1
      m=nslaves/n
c
      if(m.gt.n
     *  )then
            if(ixm.gt.iym
     *         )then
                    nx=m
                    ny=n
                else
                    nx=n
                    ny=m
              endif
         else
            if(ixm.gt.iym
     *         )then
                    nx=n
                    ny=m
                else
                    nx=m
                    ny=n
              endif
      endif
c
      return
c
      end
