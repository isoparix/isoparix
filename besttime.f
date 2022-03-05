      program parallel
c
c      How many Pentium-IIIs match N Regattas?
c
c      nr=Number of Regattas
c      ni=Number of Pentium-IIIs
c       a=Regatta performance/Intel performance
c       q=percentage of code that is serial
c
      logical runcpu
c
      write(*,102)
      read(*,*)a
c
  1   continue
c
      write(*,100)
      read(*,*)nq
c
      nintervals=40
      b=.01*float(nq)/float(nintervals)
      do n=1,nintervals
         q=float(n)*b
         pq=100.*q
         timin=q
         tr16=(q/a)+(1-q)/(16.*a)
         write(*,103)pq,timin,tr16,timin/tr16,tr16/timin
      enddo
c
      go to 1
c
100   format('Max percentage serial?')
101   format(i9,4f9.2)
102   format('Power ratio?')
103   format(5f9.2)
104   format('This workload will need',i10,' slow CPUs to match',i3
     *      ,' fast CPUs')
c
      end

