      subroutine tapewrite(nblks,nsize)
c
c      Writes a character array nblks times
c
      use dacomm
c
      real (8) txfer,txfer1,txfer2,trewind,trewind1,trewind2
c
      character (1) tapedata(nsize)
c
c      Open the file
c
      open(3,form='unformatted'
     *      ,recl=nsize
     *      ,access='sequential'
     *      ,action='write'
     *      ,file='/dev/'//adjustl(tapename)
     *      ,position='rewind')
c
      do i=1,nsize
         tapedata(i)=char(i)
      enddo
c
      call tim(txfer1)
      write(20,100)txfer1
      call isoflush(20)
c
      do i=1,nblks
         write(3)tapedata
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
      return
c
100   format('WRIT: From',f10.3,$)
1001  format(' to',f10.3,'. Elap=',f8.3,' REW: From',f10.3,$)
1002  format(' to',f10.3,'. Elap=',f8.3)
c
c
      end
