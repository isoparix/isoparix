      subroutine summer(xtemp,nmeans,kavg)
      use allcomms
c
c      Sums(kavg=0)/averages(kavg=1) every nq terms, 
c      Uses all raw data samples
c      Number of summed/averaged items returned in nmeans
c
      dimension xtemp(nline)
c
c     write(*,103)nmeans,kavg,nline
      x_min=1.e+30
      x_max=0.
      average=0.
      nmeans=0
      sum=0.
      nqa=0
      do i=1,nline
         if(xtemp(i).lt.x_min)x_min=xtemp(i)
         if(xtemp(i).gt.x_max)x_max=xtemp(i)
         sum=sum+xtemp(i)
         average=average+xtemp(i)
         nqa=nqa+1
c        write(30,102)i,t(i),xtemp(i),nqa
c
         if(nqa.eq.nq.or.i.eq.nline)then
            nmeans=nmeans+1
c
            if(kavg.eq.0)then
c
c      This is an absolute quantity, and must be added up
c
                             xtemp(nmeans)=sum
                             avgtype='  SUMMED'
                         else
c
c      This is a rate, and must be averaged over the samples.
c
                             xtemp(nmeans)=sum/float(nqa)
                             avgtype='AVERAGED'
            endif
c           write(30,101)xtemp(nmeans),nmeans,nqa
            sum=0.
            nqa=0
         endif
c 
      enddo
c
c      Write out the sample-level minima and maxima
c
      if(kavg.ne.0
     *   )then
              average=average/float(nline)
              write(20,107)title,x_max,x_min,average
      endif
c     write(*,104)nmeans,kavg,nline
c
100   format(e12.4,i4)
101   format(20x,e12.4,2i4)
102   format(i8,2e12.4,i4)
103   format('SUMMER START: NMEANS=',i8,', KAVG=',i8,', NLINE=',i8)
104   format('SUMMER END  : NMEANS=',i8,', KAVG=',i8,', NLINE=',i8)
107   format(a60,4f15.3)
      return
      end
