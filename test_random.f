      program test_random
c
c      Program to check random number generator
c
      use dacomm
c
c      Check that nblks is a suitable value for random number generation
c
      integer(8)nblks,norig,nextrnd
c
  1   continue
      write(*,103)
      read(*,*)nblks
  4   continue
      norig=nblks
      if(nblks.le.0)stop
      call kxgen(nblks,irc)
      if(irc.ne.0)go to 4
c
      write(*,108)kx,ncp,nrange
c     write(*,  *)kx,ncp,nrange
c
      write(*,104)
      read(*,*)kran
      if(kran.eq.0)stop
      if(kran.lt.0)go to 1
c
      nl=0
      do n=1,nrange
         nr=nextrnd()
         write(4,100)nr
               nl=nl+1
               write(*,105)nr+1,n
c
         if(nl.gt.nrange
     *     )then
                exit
         endif
c
      enddo
c
      write(*,102)
      go to 4
c
100   format(i6$)
101   format(6i20)
102   format(/)
103   format('Enter large number for range (0 to stop).')
104   format('Enter start seed, eg date... (0 to stop,'
     *      ,' <0 for new large number).')
105   format(i4,i15)
108   format(/'X[n+1] = (',i0.0,' * X[n] + ',i0.0,') mod ',i0.0,/)
c
      end
