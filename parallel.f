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
      q=.01*float(nq)
c
      t4=q*(a-1.)/(1.-q)
c
      do nr=1,16
         rn=nr
         denom=1./((q*(1.-a)*rn)+(1.-q))
         if(denom.gt.0)then
                           limit=a*(1.-q)*rn*denom
                           nlim=nr
         endif
      enddo
      write(*,104)limit,nlim
c
c      Adjust limit
c
      nintervals=40
      ai=.5+(float(limit)/float(nintervals))
      do n=1,nintervals
         ni=ai*float(n)
         r=(t4*float(ni))+a
         rn=float(ni)/r
         write(*,101)ni,r,rn,1./float(ni),1./rn
      enddo
c
      go to 1
c
100   format('Percentage serial?')
101   format(i9,4f9.2)
102   format('Power ratio?')
103   format(2f9.2)
104   format('This workload will need',i10,' slow CPUs to match',i3
     *      ,' fast CPUs')
c
      end

