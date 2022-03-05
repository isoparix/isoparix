      subroutine segsc(segment,segslope,segcept)
c
      implicit real(8) (a-h,o-z)
      real(8),dimension(4) :: segment
c
      segslope=slope(segment(1),segment(2),segment(3),segment(4))
      segcept =cept (segment(1),segment(2),segment(3),segment(4))
c
      return
      end
