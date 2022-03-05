      real(8) function lat2y(latitude)
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
      real(8) latitude
c
      lat2y=(latitude-ctrlat)*miles_per_deglat
c
      return
      end
