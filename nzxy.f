      subroutine nzxy(xtemp,ytemp,nsamples)
c
c      Removes data pairs with X,Y<=0.
c
      use allcomms
      dimension xtemp(nline),ytemp(nline)
c
      nsamples=0
      do i=1,nline
         if(ytemp(i).gt.0..and.xtemp(i).gt.0.)then
                            nsamples=nsamples+1
                            xdata(nsamples)=xtemp(i)
                            ydata(nsamples)=ytemp(i)
         endif
      enddo 
      return
      end
