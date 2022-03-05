      subroutine sdev(xtemp,nsamples)
c
c      Calculate mean and SD for n values of xtemp
c
      use allcomms
      real (8) sigx,sigx2
      dimension xtemp(nsamples)
c
      delta=.0001
      ncells=15
      amin=huge(amin)
      amax=0.
      sigx=0.
      sigx2=0.
      do j=1,nsamples
                  sigx =sigx + xtemp(j)
                  sigx2=sigx2+(xtemp(j)**2)
                  if(xtemp(j).gt.amax)amax=xtemp(j)
                  if(xtemp(j).lt.amin)amin=xtemp(j)
      enddo
c
c      Calculate means and standard deviations...
c
      mux=sigx/float(nsamples)
      sdx=sqrt((sigx2/float(nsamples))-mux**2)
c
c     write( 8,100)
c    *nsamples,mux,sdx,amin,amax,sigx,sdx/mux
c
c      Write histogram details
c
      dspan=float(ncells)/((1.+delta)*(amax-amin))
c
      do k=0,ncells-1
         khist(k)=0
      enddo
c
      do j=1,nsamples
         kcell=(xtemp(j)-amin)*dspan
         if(kcell.ge.0.and.kcell.le.14
     *     )then
                khist(kcell)=khist(kcell)+1
         endif
      enddo
c
c     write(10,101)amin,amax
c    *            ,(khist(mx),mx=0,ncells-1)
c     call flush(10)
c
      return
c
100   format(i8,6e15.6)
101   format(2e10.3,20i6)
c
      end
