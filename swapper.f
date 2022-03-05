      program swapper
c
c      Interchanges two power columns, eg X for Y, X for Z, etc
c
      implicit real *8 (a-h,o-z)
      character *1 avar, bvar
c
c      Choose which columns to swap
c
  3   continue
      write(*,101)
      read(*,*)avar,bvar
c
      nswap=0
      if(avar.eq.'x'.and.bvar.eq.'y')nswap=1
      if(avar.eq.'x'.and.bvar.eq.'z')nswap=2
      if(avar.eq.'y'.and.bvar.eq.'z')nswap=3
      if(nswap.eq.0
     *  )then
             go to 3
      endif
c
  1   continue
      read(3,*,end=2)k,c
c
      ix=k/100
      k=k-(ix*100)
      iy=k/10
      iz=k-(iy*10)
c
      if(nswap.eq.1)write(8,100)iy,ix,iz,c
      if(nswap.eq.2)write(8,100)iz,iy,ix,c
      if(nswap.eq.3)write(8,100)ix,iz,iy,c
c
      go to 1
  2   continue
c
      stop
c
100   format(3i1,f8.1)
101   format('Choose to swap x/y/z with another column...  eg x z')
102   format(3i1,f0.0)
c
      end
