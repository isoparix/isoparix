      program test_side
c
c   Place n rectangles in a box...
c
  1   continue
      write(*,100)
      read(*,*)ixm,iym
      if(ixm*iym.le.0)stop
c
  2   continue
      write(*,101)
      read(*,*)nslaves
      if(nslaves.le.0)go to 1
c
      call side_calc(ixm,iym,nslaves,nx,ny)
      write(*,*)nx,ny
      go to 2
c
100   format('Enter ixm and iym on same line - zero to end')
101   format('Enter nslaves - zero for new ixm and iym')
      end
