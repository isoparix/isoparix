      real(8) function lon2x(longitude)
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
      real(8) longitude
c
      lon2x=(longitude-ctrlon)*miles_per_deglon
c
      return
      end
