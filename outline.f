      subroutine outline
c
c      Draws a boundary of the outer XY coords
c
      use isocomm
c
      call x11bisect(%val(ixcmax),%val(iycmax)
     *              ,%val(ixcmax),%val(iycmin),%val(nedgecol))
      call x11bisect(%val(ixcmin),%val(iycmax)
     *              ,%val(ixcmin),%val(iycmin),%val(nedgecol))
      call x11bisect(%val(ixcmax),%val(iycmax)
     *              ,%val(ixcmin),%val(iycmax),%val(nedgecol))
      call x11bisect(%val(ixcmax),%val(iycmin)
     *              ,%val(ixcmin),%val(iycmin),%val(nedgecol))
c     write(*,105)ixcmax,iycmax,ixcmin,iycmin,nedgecol
c
      return
c
105   format('Screen max X/Y:',2i7,', screen min X/Y:',3i7)
      end
