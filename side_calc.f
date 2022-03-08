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
      if(nx*ny.lt.nslaves-2   ! N(processes) -Master -Artist
     *  )then
             if(nx.lt.ny)nx=nx+1
         else
             if(ny.le.nx)ny=ny+1
      endif 
c      
      write(0,100)ixm,iym,nslaves,nx,ny
100   format('SIDE_CALC: IXM=',i0.0,', IYM=',i0.0,', NSLAVES=',i0.0
     *,', NX=',i0.0,', NY=',i0.0)      
c
      return
c
      end
