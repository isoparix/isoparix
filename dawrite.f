      subroutine dawrite(filename,nfirst,nblks,nstep,nsize,txfer,dr)
c
c      Writes a character array nblks times
c
      integer (8) nfirst,nblks,nstep,ntotal
c
      real (8) ta,tb,tstart,tend,topen,tclose,txfer
c
      character (1) dadata(nsize)
      character (50) filename
c
      irc=0
      do i=1,nsize
         dadata(i)=char(i)
      enddo
c
      ntotal=0
      call tim(tstart)
      do i=nfirst,nblks,nstep
         ntotal=ntotal+1
         write(3,rec=i)dadata
      enddo
      call isoflush(3)
      call system('sync')
c     call system('sync',irc)
c
      if(irc.ne.0
     *  )then
             write(0,101)irc
             write(*,101)irc
      endif
c
      call tim(tend)
c
      call da_txfer(tstart,tend,ntotal,nsize,txfer,dr)
c
      return
c
101   format('***** ERROR IN DAWRITE: Result code -',i6,' running sync')
c
      end
