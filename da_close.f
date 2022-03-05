      subroutine da_close(tclose)
c
c      Closes unit 3
c
      real (8) tclose,tb,tdelta
c
      call tim(tclose)
      close(3,err=99)
      call tim(tb)
      tclose=tdelta(tb,tclose)
c     write(*,101)tclose
c
      return
c
 99   continue
      write(0,100)
      stop
c
100   format('***** ERROR in DA_CLOSE:  Could not close unit 3')
101   format('Unit 3 closed in',f10.4,' seconds')
c
      end
