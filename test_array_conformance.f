      program test
c
c      Tests array conformance
c
      integer array(100)
c
      do i=1,100
         array(i)=i
      enddo
c
  1   continue
      write(*,100)
      read(*,*)istart,ilength
      if(istart.eq.0)stop
      call writeout(array(istart:istart+ilength-1),ilength)
      go to 1
c
100   format('Start index and length of transferred array?')
      end
c
c
c
      subroutine writeout(extract,ilength)
c
      integer extract(ilength)
c
      write(*,*)extract
c
      return
      end
