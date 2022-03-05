      program fingather
c
c      Sums grepped output from * in ungathered.dat format
c      eg
c           08 08 Aug 2011 BAC    -2457.79  Income: eg IBM UK PENSIONS
c
      implicit real(8) (a-h,o-z)      
c      
      items=0
      sum=0.
      write(*,106)
  1   continue
      read (*,101,end=2,err=2)date,category,value
      write(*,101            )date,category,value
      sum=sum+value
      items=items+1
      go to 1
  2   continue
      if(items.gt.1
     *  )then
             write(*,104)items,sum/float(items),sum
      endif
c             
      stop      
c      
101   format(a14,a4,f12.2,2x,a)
102   format(a14,a4,f15.2)
104   format(i8,2f11.2)
106   format(/)
c
      end      
