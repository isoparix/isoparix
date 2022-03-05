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
      call ddwrite(filename,nblks,nsize,topen,tclose,txfer,dr)
      call tim(trecord)
      write(6,115)npass,nblks,nsize,nblks*nsize
     *           ,trecord,txfer,dr,topen,tclose
c
      if(mounter
     *  )then
             call unremount(filesystem,mountdata)
      endif
c
      call ddread(filename,nblks,nsize,topen,tclose,txfer,dr)
      call tim(trecord)
c
      call system('rm '//trim(filename),irc)
      if(irc.ne.0
     *  )then
             write(0,101)trim(filename)
      endif
      write(6,115)npass,nblks,nsize,nblks*nsize
     *           ,trecord,txfer,dr,topen,tclose
      npass=npass+1
c
      go to 4
c
 99   continue
c
      stop
c
100   format(i9)
101   format('***** WARNING in DDREPWS.EXE - cannot remove ',a)
105   format(a50)
109   format(l1)
115   format(i6,3i12,f10.3,f15.3,f11.1,2f8.3)
118   format('     RANDOM        :'
     *  ,i8,' blocks of',i8,' bytes is too few for random operation')
c
      end
