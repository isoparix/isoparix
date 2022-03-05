      subroutine darread(filename,nblks,nsize,txfer,dr,nread)
c
c      Reads the data by direct access in truly random fashion
c
      use dacomm
c
      integer (8) nextrnd,nblks,nread
c
      real (8) tstart,tend,telap,topen,txfer
c
      character (1) dadata(nsize)
      character (50) filename
c
      call tim(tstart)
      do i=1,nread
         read(3,rec=nextrnd)dadata
      enddo
      call tim(tend)
c
      call da_txfer(tstart,tend,nread,nsize,txfer,dr)
c
      return
      end
