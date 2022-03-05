      program test_dist
c
c      Tests distance from point to line
c
      implicit real(8) (a-h,o-z)
c
      real(8),dimension(4) :: segment
c
  1   continue
      write(*,101)
      read(*,*)x,y,x1,y1,x2,y2
      segment(1)=x1
      segment(2)=y1
      segment(3)=x2
      segment(4)=y2
      write(*,100)x,y,segment,d_pointline(x,y,segment)
      go to 1
      stop
100   format(10f15.6)
101   format('x,y,x1,y1,x2,y2')
      end
