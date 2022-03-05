      program darepu
c
c      Program to measure speed of direct
c      random updates to fort.3
c
      use dacomm
c
      character (50) filesystem
      character (50) filename
      character (50) mountdata
      character (10) timename
c
      integer (8) n,nblks,nread
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
c      Check that nblks is a suitable value for random number generation
c
      call kxgen(nblks,irc)
c
c      Write, read the file
c
      call dawrite(filename,1_8,nblks,1_8,nsize,txfer,dr)
c
      if(irc.eq.0
     *  )then
             if(nblks.gt.1000
     *         )then
                    nread=1000
                else
                    nread=nblks
             endif
         else
             write( *,118)nblks,nsize
      endif
c
c      Use this step for continuous operation
c
      npass=0
      call tim(trecord)
      write(6,115)npass,nblks,nsize,nblks*nsize
     *           ,trecord,txfer,dr,topen,tclose
  4   continue
c
      if(mounter
     *  )then
             call unremount(filesystem,mountdata)
      endif
c
      call darupdt(filename,nblks,nsize,txfer,dr,nread)
      call tim(trecord)
      npass=npass+1
      write(6,115)npass,kran,nsize,nblks*nsize
     *           ,trecord,txfer,dr,topen,tclose
c
      go to 4
c
 99   continue
c
      stop
c
100   format(i9)
105   format(a50)
109   format(l1)
115   format(i6,3i12,f10.3,f15.3,f11.1,2f8.3)
118   format('     RANDOM        :'
     *  ,i8,' blocks of',i8,' bytes is too few for random operation')
c
      end
