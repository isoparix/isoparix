c
c
c
      subroutine stddev(filename,attrib,x,nsamples)
c
c      Calculate mean and SD for n values of x
c
      implicit real (8) (a-h,o-z)
      real (8) mux
      parameter (ncells=10)
      dimension filename(nsamples),x(nsamples),khist(0:ncells-1)
c
      character (12) attrib
      character (49) filename
      character (68) textline
c
      delta=.0001
      xmin=huge(xmin)
      xmax=0.
      sigx=0.
      sigx2=0.
      do j=1,nsamples
                  sigx =sigx + x(j)
                  sigx2=sigx2+(x(j)**2)
                  if(x(j).gt.xmax)xmax=x(j)
                  if(x(j).lt.xmin)xmin=x(j)
                  write(80,108)filename(j),x(j)
      enddo
      close(80)
c
c      Calculate means and standard deviations...
c
      mux=sigx/float(nsamples)
      if(sigx2/float(nsamples).gt.mux**2)then
                                sdx=sqrt((sigx2/float(nsamples))-mux**2)
                                         else
                                sdx=0.
      endif
c
      write( 8,100)attrib,mux,sdx,xmin,xmax,sigx
c
c      Do the percentile predictions....
c
c     if(attrib.eq.'Average    ')then
c                       call predict(mux,sdx,nsamples,nsamples)
c     endif
c
c      Write histogram details
c
      do k=0,ncells-1
         khist(k)=0
      enddo
c
      write(10,101)xmin,xmax,mux,sigx
      if(xmax.eq.xmin)then
                          write(10,106)
                          return
      endif
c
      write(10,1011)
c
      dspan=float(ncells)/((1.+delta)*(xmax-xmin))
      do j=1,nsamples
         kcell=(x(j)-xmin)*dspan
         khist(kcell)=khist(kcell)+1
         if(kcell.ge.ncells.or.kcell.lt.0)
     *                                 write(*,102)x(j),kcell,ncells-1
      enddo
      write(10,104)(khist(mx),mx=0,ncells-1)
      write(10,105)
c
      call system('sort -n -k2 fort.80 >summan.srt')
      open(80,file='summan.srt',form='formatted',status='old')
  2   continue
         read (80,109,end=3)textline
         write(10,109)textline
         go to 2
  3   continue
      close(80)
      write(10,1012)
c
      return
c
100   format(a12,8x,5f15.3)
101   format(//' Minimum=',f18.5
     *       ,/' Maximum=',f18.5
     *       ,/' Average=',f18.5
     *       ,/'   Total=',f18.5
     *       ,/)
1011  format('.bf gt12')
1012  format('.pf')
102   format('***** ERROR in stddev: Value',f12.5
     *      ,' allocated to cell',i3,'. Permissible 0 to',i3)
103   format('Processing',i6,' samples for ',a12)
104   format(/'Distribution between min and max values',//20i4,/)
105   format(/)
106   format(' No histogram - all values are identical!')
108   format(a49,f20.5)
109   format(a68)
c
      end
