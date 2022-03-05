      module surf_comms
c
      logical,dimension(0:9,0:9,0:9) :: used  ! The cells that are used 
c
      real(8),dimension(0:9,0:9,0:9) :: coeff ! The cells of the surface
      real(8),dimension(0:28,0:28)   :: bc    ! Binary co-efficients
c
      integer ixmax,iymax,izmax
c
      end
