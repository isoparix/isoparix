      subroutine taperead(nblks,nsize)
c
      use dacomm
c
c      Reads the data on the tape
c
      real (8) txfer,txfer1,txfer2,trewind,trewind1,trewind2
c
c
      character (1) tapedata(nsize)
c
c      Open the file, and read it back
c
      open(3,form='unformatted'
     *      ,recl=nsize
     *      ,access='sequential'
     *      ,action='read'
     *      ,file='/dev/'//adjustl(tapename)
     *      ,position='rewind')
c
      call tim(txfer1)
      write(20,100)txfer1
      call isoflush(20)
c
      do i=1,nblks
         read(3)tapedata
      enddo
c
      call tim(txfer2)
      txfer=txfer2-txfer1
      call tim(trewind1)
      write(20,1001)txfer2,txfer,trewind1
      call isoflush(20)
c
      rewind(3)
      close(3)
c
      call tim(trewind2)
      trewind=trewind2-trewind1
      write(20,1002)trewind2,trewind
      call isoflush(20)
c
      return
c
100   format('READ: From',f10.3,$)
1001  format(' to',f10.3,'. Elap=',f8.3,' REW: From',f10.3,$)
1002  format(' to',f10.3,'. Elap=',f8.3)
c
      end      
