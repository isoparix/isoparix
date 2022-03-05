      subroutine memsmooth(xtemp,ytemp,nsamples)
c
c      Picks only the lowest value of xtemp associated with ytemp
c
      use allcomms
      dimension xtemp(nline),ytemp(nline)
      dimension nx(nline)
c
c     do i=1,npf
c        nx(i)=0
c        xdata(i)=10000000.
c     enddo
c
      nx=0
      xdata=100000000.
      minp=20000000
      maxp=0
      nsamples=0
c     write(*,101)nline
      do i=1,nline
         if(ytemp(i).gt.0.
     *     )then
                im=ytemp(i)
                if(im.le.nsamples
     *            )then
                       if(im.gt.maxp)maxp=im
                       if(im.lt.maxp)minp=im
                       nx(im)=nx(im)+1
                       if(xtemp(i).lt.xdata(im)
     *                   )then
                              xdata(im)=xtemp(i)
                       endif
                endif
         endif
      enddo
c
      do i=minp,maxp
         if(nx(i).gt.0)then
                           nsamples=nsamples+1
                           xdata(nsamples)=xdata(i)
                           ydata(nsamples)=i
         endif
      enddo
c
c     nchann=lsqcount+30
c     do i=1,nsamples
c        write(nchann,100)xdata(i),ydata(i)
c     enddo
c
      return
c
100   format(2f15.1)
101   format('MEMSMOOTH: NLINE=',i10)
c
      end
