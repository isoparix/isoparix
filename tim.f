      subroutine tim(t)
c
c      returns time of day in real seconds on aix machines
c
      integer ctim
      real (8) t
      idummy=ctim(isec,nsec)
      t=float(isec)+(1.e-09*float(nsec))
      return
      end
