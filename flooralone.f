      subroutine flooralone
c
c      Paint the floor on its own
c
      use isocomm
c
      ixcmax=0
      ixcmin=10000
      iycmax=0
      iycmin=10000
c
      do nc=1,ncubes
                    call drawface(nc,5,facecol(nc),3)
      enddo
      call outline
c
      return
      end
