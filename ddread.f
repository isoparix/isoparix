      subroutine ddread(filename,nblks,nsize,topen,tclose,txfer,dr)
c
c      Reads the data by direct access in sequential fashion
c
      integer (8) nblks
c
      real (8) ta,tb,tstart,tend,topen,tclose,txfer
c
      character (9) bs,count
      character (132)dd_command
      character (50) filename
c
      topen=0.
      tclose=0.
      write(bs,*)nsize
      write(count,*)nblks
      dd_command='dd of=/dev/null if='//trim(filename)//
     *           ' bs='//   trim(adjustl(bs))//
     *           ' count='//trim(adjustl(count))
     *         //' 2>/dev/null'
      call tim(tstart)
      call system(dd_command,irc)
c
      if(irc.ne.0
     *  )then
             write(0,100)irc
             write(*,100)irc
      endif
c
      call tim(tend)
      call da_txfer(tstart,tend,nblks,nsize,txfer,dr)
c
      return
c
100   format('***** ERROR IN DDREAD: Result code -',i6,' running dd')
c
      end      
