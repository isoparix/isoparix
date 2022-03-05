      real(8) function x2lon(x)
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      x2lon=(x*miles_per_line/miles_per_deglon)+ctrlon
c
      return
      end
