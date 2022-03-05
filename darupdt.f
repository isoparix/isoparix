      subroutine darupdt(filename,nblks,nsize,txfer,dr,nread)
c
c      Updates the data by direct access in truly random fashion
c
      use dacomm
c
      integer (8) n,nblks,nread,nextrnd
c
      real (8) tstart,tend,telap,tclose,topen,txfer
c
      character (1) dadata(nsize)
      character (50) filename
c
      do i=1,nsize
         dadata(i)=char(mod(2*i+koff,128))
      enddo
c
      kran=1
c
      call tim(tstart)
c
      do i=1,nread
         write(3,rec=nextrnd)dadata
      enddo
c
      call isoflush(3)
      call system('sync',irc)
c
      if(irc.ne.0
     *  )then
             write(0,101)irc
             write(*,101)irc
      endif
c
      call tim(tend)
c
      call da_txfer(tstart,tend,nread,nsize,txfer,dr)
c
      return
c
101   format('***** ERROR IN DARUPDT: Result code -',i6,' running sync')
c
      end
