      subroutine filler(ix1,iy1,ix2,iy2,miniter)
c
c      Records a filled area defined by the parameters with the
c      value of miniter
c
      use isocomm
c
      ixa=ix1+1
      ixb=ix2-1
      iya=iy1+1
      iyb=iy2-1
c
c      Record filled rectangle
c
      call rectput(ixa,iya,ixb,iyb,miniter)
      return
      end
