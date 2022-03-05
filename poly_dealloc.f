      subroutine poly_dealloc
c
c      Allocates space
c
      use andrew_comms
c
      implicit real(8) (a-h,o-z)
c
      deallocate(line_view)
      deallocate(depth)
      deallocate(xstart)
      deallocate(ystart)
      deallocate(xend)
      deallocate(yend)
c
      return
      end
