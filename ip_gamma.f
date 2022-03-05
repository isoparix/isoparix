      subroutine ip_gamma(gamma)
c
      use bmp_comms
      use ip_comms
c
c      Enhance.   Assume that data in brightness is normalised and
c      distributed.   can be raised to a real gamma
c
      write(*,100)gamma
      do iy=0,iydim-1
         do ix=0,ixdim-1
            brightness(ix,iy)=brightness(ix,iy)**gamma
            level=0.5+(255.*(brightness(ix,iy)))
c
            if(level.lt.0
     *        )then
                   write(*,*)level,ix,iy,brightness(ix,iy),gamma
                   level=0
            endif
c
            if(level.gt.255
     *        )then
                   write(*,*)level,ix,iy,brightness(ix,iy),gamma
                   level=255
            endif
c
            picdata(ix,iy)=char(level)
         enddo
      enddo
c
      return
c
100   format('Applying gamma of',f7.3)
c
      end
