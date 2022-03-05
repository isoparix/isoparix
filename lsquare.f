      subroutine lsquare(xtemp,ytemp,nsamples)
c
      use allcomms
      dimension xtemp(nline),ytemp(nline)
      real (8) sigx,sigy,sigx2,sigy2,sigxy,tx,txd,tyd,asamples
      real (8) rx,rx2,slope
c
      logical anchor
c
      lsqcount=lsqcount+1
c
      sigx =0.
      sigy =0.
      sigx2=0.
      sigy2=0.
      sigxy=0.
      nzx=0
      nzy=0
      mux=0.
      muy=0.
      sdx=0.
      sdy=0.
c
c     write(*,105)xname,lsqcount,yname,nsamples,nline
c
      if(nsamples.lt.0
     *  )then
             anchor=.true.
             nsamples=-nsamples
         else
             anchor=.false.
      endif
      asamples=nsamples
c
      if(nsamples.lt.2)then
                   write(14,100)xname,lsqcount,yname,nsamples
                   return
      endif
c
      do j=1,nsamples
                  sigx =sigx + xtemp(j)
                  sigy =sigy + ytemp(j)
                  sigx2=sigx2+(xtemp(j)**2)
                  sigy2=sigy2+(ytemp(j)**2)   
                  sigxy=sigxy+(ytemp(j)*xtemp(j))
                  if(xtemp(j).gt.0.)nzx=nzx+1
                  if(ytemp(j).gt.0.)nzy=nzy+1
      enddo
c     write(*,*)nzx,nzy
c
c      Check for realistic data
c
      if((nzx.lt.2).or.(nzy.lt.2))then
                  write(14,106)xname,nzx,lsqcount,yname,nzy
                  return
      endif
c 
c      Calculate means and standard deviations...
c
      mux=sigx/asamples
      muy=sigy/asamples
      sdx=sqrt((sigx2/asamples)-mux**2)
      sdy=sqrt((sigy2/asamples)-muy**2)
c
c     write(*,804)nsamples,sigx,sigx2,sigy,sigy2,sigxy
804   format(i6,5e13.5)
c
c      Calculate correlation coefficients 
c
        zero=0.
        txd=asamples*sigx2-sigx*sigx
        if(txd.ne.0.
     *    )then
               tyd=asamples*sigy2-sigy*sigy
               if(tyd.ne.0.
     *           )then
                      tx=asamples*sigxy-sigx*sigy
                      slope =tx/txd
                      slopef=sigxy/sigx2
                      ycept =((sigy*sigx2)-(sigx*sigxy))/txd
                      xcept =-ycept/slope
                      rx2=tx*tx/(txd*tyd)
                      rx =sign(dsqrt(rx2),slope)
                      if(anchor
     *                  )then
                            write(14,1011)
     *         lsqcount,xname,mux,sdx,slopef
     *        ,lsqcount,yname,nsamples,muy,sdy,slope,ycept,xcept,rx,rx2
                         else
                             write(14,101 )
     *         lsqcount,xname,mux,sdx
     *        ,lsqcount,yname,nsamples,muy,sdy,slope,ycept,xcept,rx,rx2
                      endif
               endif
        endif
c
c      Compare actuals and least-squares fit
c
c     write(14,104)
c     do i=1,nsamples
c        a=(slope*xtemp(i))+ycept
c        write(14,103)i,xtemp(i),ytemp(i),a
c     enddo
c
      return
c
100   format(/,'    X: ',a30
     *      ,/ i3,' Y: ',a30,i6,' Too few samples to process')
101   format(/ i3,' X: ',a30,6x,2e12.5
     *      ,/ i3,' Y: ',a30,i6,5e12.5,2f7.3)
1011  format(/ i3,' X: ',a30,6x,3e12.5,'     (Forced origin)'
     *      ,/ i3,' Y: ',a30,i6,5e12.5,2f7.3)
103   format(i12,3f20.6)
104   format('   Sample ID       X               Y'
     *      ,'   Predicted Y')              
105   format(/,'    X: ',a30
     *      ,/ i3,' Y: ',a30,' Samples=',i6,' NLINE=',i6)
106   format(/,'    X: ',a30,' Non-zero samples=',i7
     *      ,/ i3,' Y: ',a30,' Non-zero samples=',i7)
      end
