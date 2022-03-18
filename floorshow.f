      subroutine floorshow
c
c      Paint the floor... (removing this routine doesn't performance)
c
      use isocomm
c
      do nc=1,ncubes
         if(.not.active(nc)
     *     )then
c               write(0,100)nc,facecol(nc)
                call drawface(nc,5,facecol(nc),3)
         endif
      enddo
c
      return
100   format('FLOORSHOW: Cube',i3,', colour',i4)
      end
