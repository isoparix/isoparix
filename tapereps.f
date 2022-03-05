      program tapetest
c
c      Program to do repeated sequential reads from tape
c
      use dacomm
c
      logical set_tape_bs
c
      integer (8) n
      real (8) trecord
c
      character (12) cnsize
      character (20) cblock
      character (72) chdev
c
c      Read the tapename
c
      write(*,103)
      read(*,*)tapename
      write(*,104)tapename
c
c      Read the desired file size
c
      write(*,102)
      read(*,*)n
c
  1   continue
c
c      Read the desired block size
c
      write(*,101)
      read(*,*)nsize
      if(nsize.eq.0)stop
      if(nsize.lt.0
     *  )then
             nsize=-nsize
             set_tape_bs=.true.
         else
             set_tape_bs=.false.
      endif
      nblks=n/nsize
      if(nblks.le.0
     *  )then
             write(*,108)
             go to 1
      endif
c
      if(set_tape_bs
     *  )then
c
c      Set the block size on the tape drive
c
             write(cnsize,107)nsize
             write(chdev,105)trim(adjustl(tapename))
     *                      ,trim(adjustl(cnsize))
             write(*,109)trim(adjustl(chdev))
             call system(chdev)
      endif
c
c      Start continuous operation
c
      open(20,file=trim(adjustl(tapename))//'.times',status='replace')
      npass=0
      call tim(trecord)
      write(*,115)npass,trecord
c
c      Write the file
c
      call tapewrite(nblks,nsize)
      npass=npass+1
      call tim(trecord)
      write(*,115)npass,trecord
c
  4   continue
c
c      Read the file
c
      call taperead(nblks,nsize)
c
      npass=npass+1
      call tim(trecord)
      write(*,115)npass,trecord
      go to 4
c
100   format(i9)
101   format('Block size (0=stop, -ve value to change drive setting)?')
102   format('Desired file size?')
103   format('Name of tape device (eg for /dev/rmt1 answer rmt1)?')
104   format('Tape device is ',a12)
105   format('chdev -l ',a,' -a compress=no -a block_size=',a)
106   format(a12)
107   format(i12)
108   format('File size smaller than block size... Please re-enter')
109   format(/a,/)
c
115   format(i6,36x,f10.3,'           .            .')
c
      end
