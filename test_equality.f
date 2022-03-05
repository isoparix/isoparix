      program test
c
      logical la, lb
c
  1   continue
c
      read(*,*)i,j,k
      if(i*j*k.lt.0)stop
c
      if(i.eq.j
     *  )then
             la=.true.
         else
             la=.false.
      endif
c
      if(j.eq.k
     *  )then
             lb=.true.
         else
             lb=.false.
      endif
c
      if(la.eq.lb
     *  )then
             write(*,100)
         else
             write(*,101)
      endif
c
      go to 1
c
100   format('All numbers equal')
101   format('Some inequalities')
c
      end
