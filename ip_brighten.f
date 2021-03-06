      subroutine ip_brighten(brighten)
c
c      Lightens or darkens picture data
c
      use ip_comms
c
      real(8)brighten,b
c
      write(*,100)brighten
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            b=brighten*brightness(ix,iy)
            brightness(ix,iy)=amin1(1.0,b)
            picdata(ix,iy)=char(int(0.5+(255.0*brightness(ix,iy))))
         enddo
      enddo
c
      return
c
100   format('Brightening by',f12.5)
c
      end
