      real(8) function y2lat(y)
c
      use venn_comms
c
      implicit real(8) (a-h,o-z)
c
      y2lat=(y*miles_per_line/miles_per_deglat)+ctrlat
c
      return
      end
