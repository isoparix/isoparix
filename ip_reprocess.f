      program ip_reprocess
c
c
c
      use ip_comms
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      open(10,'blurred.dat')
      read(10),ixdim,iydim
      allocate(ipic    (0:ixdim-1,0:iydim-1))
      allocate(conarray(0:ixdim-1,0:iydim-1))
      read(10)ipic
      read(10)conarray
c
c
      stop
      end
