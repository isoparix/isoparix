      subroutine ip_floor
c
c      Subtracts levelmin from the levels of the data in a character array.  
c      Zero is left at zero
c
c      The character array picdata has values between 0 and 255.
c
      use bmp_comms
      use ip_comms
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            level=ichar(picdata(ix,iy))
            if(level.le.nthresh
     *        )then
                   picdata(ix,iy)=char(0)
            endif
         enddo
      enddo
      return
c
      end
