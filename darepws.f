      program darepw
c
c      Program to measure speed of direct
c      sequential writes to and reads from fort.3
c
      use dacomm
c
      character (50) filesystem
      character (50) filename
      character (50) mountdata
      character (10) timename
c
      integer (8) n,nblks
c
      real (8) trecord,topen,tclose,txfer
c
      logical mounter
c
      call da_init(mounter,filesystem,filename,n,mountdata)
c
  1   continue
c
c      Read the desired block size
c
      call da_blksize(n,nblks,nsize) 
      if(nsize.le.0.or.
     *   nblks.le.0
     *  )then
             stop
      endif
c
c      Write, read, delete the file over and over again...
c
      npass=0
  4   continue
c
      call da_open(filename,nsize,'write ',topen)
      call dawrite(filename,1_8,nblks,1_8,nsize,txfer,dr)
      call da_close(tclose)
      call tim(trecord)
      write(6,115)npass,nblks,nsize,nblks*nsize
     *           ,trecord,txfer,dr,topen,tclose
      npass=npass+1
c
      if(mounter
     *  )then
             call unremount(filesystem,mountdata)
      endif
c
      call da_open(filename,nsize,'read  ',topen)
      call dasread(filename,1_8,nblks,1_8,nsize,txfer,dr)
      call da_close(tclose)
      call tim(trecord)
c
      write(6,115)npass,nblks,nsize,nblks*nsize
     *           ,trecord,txfer,-dr,topen,tclose
      npass=npass+1
c
      call system('rm '//trim(filename),irc)
      if(irc.ne.0
     *  )then
             write(*,101)trim(filename)
         else
             write(*,102)trim(filename)
      endif
c
      go to 4
c
 99   continue
c
      stop
c
100   format(i9)
101   format('***** WARNING in DAREPWS.EXE - cannot remove ',a)
102   format('File ',a,' has been deleted')
105   format(a50)
109   format(l1)
115   format(i6,3i12,f10.3,f15.3,f11.1,2f8.3)
c
      end
