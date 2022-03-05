      subroutine binomial
c
c      calculate the binary coefficients
c
      use surf_comms
c
      bc=0.0
      bc(0,:)=1.0
      bc(1,1)=1.0
      iy=1
      do iy=2,9
         do ix=1,iy
            bc(ix,iy)=bc(ix,iy-1)+bc(ix-1,iy-1)
         enddo
c        write(*,104)iy,(bc(mx,iy),mx=0,iy)
      enddo
c
      return
c
104   format(i4,': ',12f12.1)
c
      end
