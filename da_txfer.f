      subroutine da_txfer(tstart,tend,nblks,nsize,txfer,dr)
c
c      Computes data rates
c
      integer (8) nblks
c
      real (8) tstart,tend,txfer,tdelta,ablks
c
      txfer=tdelta(tend,tstart)
      ablks=nblks
      dr=ablks*float(nsize)/(1000.*txfer)
c
c     write(*,100)tstart,tend,nblks,nsize,txfer,dr
c
      return
c
100   format(/'      Started:',f12.3
     *      ,/'        Ended:',f12.3
     *      ,/'         File:',i12,' blocks of',i10,' bytes'
     *      ,/'Transfer time:',f12.3,' seconds'
     *      ,/'Transfer rate:',f12.3,' kB/S'
     *      )
c
      end
