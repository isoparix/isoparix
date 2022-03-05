      program datest
c
c      Program to measure speed of direct
c      I/O to fort.3
c
      character (50) filesystem
      character (50) filename
      character (50) mountdata
      character (10) timename
      character ( 6) mode
c
      integer (8) n,nblks,nread
c
      real (8) trecord,topen,tclose,txfer
c
      logical mounter
c
      call da_init(mounter,filesystem,filename,n,mountdata)
      write( 6,104)trim(filename),n
      write(10,104)trim(filename),n
      write(12,104)trim(filename),n
      write(14,104)trim(filename),n
      write(16,104)trim(filename),n
c
      write(10,210)
      write(12,212)
      write(14,214)
      write(16,216)
      write(18,218)
      write(20,220)
c
  1   continue
      write(*,117)
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
c      Write, read the file,using FORTRAN direct access
c
      mode='write '
      call da_open(filename,nsize,mode,topen)
      call dawrite(filename,1_8,nblks,1_8,nsize,txfer,dr)
      call da_close(tclose)
      write( *,120)nblks,nsize,txfer,dr,topen,tclose
      write(10,120)nblks,nsize,txfer,dr,topen,tclose
c
      if(mounter
     *  )then
             call unremount(filesystem,mountdata)
      endif
c
      mode='read  '
      call da_open(filename,nsize,mode,topen)
      call dasread(filename,1_8,nblks,1_8,nsize,txfer,dr)
      call da_close(tclose)
      write( *,122)nblks,nsize,txfer,dr,topen,tclose
      write(12,122)nblks,nsize,txfer,dr,topen,tclose
c
      if(irc.eq.0
     *  )then
             if(nblks.gt.1000
     *         )then
                    nread=1000
                else
                    nread=nblks
             endif
c
             if(mounter
     *         )then
                    call unremount(filesystem,mountdata)
             endif
c
             mode='read  '
             call da_open(filename,nsize,mode,topen)
             call darread(filename,nblks,nsize,txfer,dr,nread)
             call da_close(tclose)
             write( *,124)nread,nsize,txfer,dr,topen,tclose
             write(14,124)nread,nsize,txfer,dr,topen,tclose
c
             if(mounter
     *         )then
                    call unremount(filesystem,mountdata)
             endif
c
             mode='update'
             call da_open(filename,nsize,mode,topen)
             call darupdt(filename,nblks,nsize,txfer,dr,nread)
             call da_close(tclose)
             write( *,126)nread,nsize,txfer,dr,topen,tclose
             write(16,126)nread,nsize,txfer,dr,topen,tclose
         else
             write( *,118)nblks,nsize
             write(14,118)nblks,nsize
             write(16,118)nblks,nsize
      endif
c
      call system('rm '//filename,irc)
c
c      Write, read the file,using dd sequential
c
      call ddwrite(filename,nblks,nsize,topen,tclose,txfer,dr)
      write( *,128)nblks,nsize,txfer,dr,topen,tclose
      write(18,128)nblks,nsize,txfer,dr,topen,tclose
c
      if(mounter
     *  )then
             call unremount(filesystem,mountdata)
      endif
c
      call ddread(filename,nblks,nsize,topen,tclose,txfer,dr)
      write( *,130)nblks,nsize,txfer,dr,topen,tclose
      write(20,130)nblks,nsize,txfer,dr,topen,tclose
c
      call system('rm '//filename,irc)
c
c      Make sure we get something if we crash....
c
      call isoflush(10)
      call isoflush(10)
      call isoflush(12)
      call isoflush(14)
      call isoflush(16)
      call isoflush(18)
      call isoflush(20)
c
      go to 1
c
 99   continue
c
      stop
c
100   format(i9)
101   format('Block size on da?')
102   format('Desired file size?')
103   format('Filesystem name?')
104   format(a40,' of approx ',i12,' bytes is test file')
105   format(a50)
106   format('No filesystem will be unmounted')
107   format('Program will un/mount filesystem ',a50)
108   format('Will filesystem be un/mounted? T or F')
109   format(l1)
117   format(/)
118   format('     RANDOM        :'
     *  ,i8,' blocks of',i8,' bytes is too few for random operation')
120   format(' SEQUENTIAL WRITES :'
     *      ,i8,' blocks of',i8
     *      ,' bytes writ in',f7.2,' S, at',f9.1
     *      ,' KB/S.  Open',f6.3,', close',f6.3,' seconds')
122   format(' SEQUENTIAL READS  :'
     *      ,i8,' blocks of',i8
     *      ,' bytes read in',f7.2,' S, at',f9.1
     *      ,' KB/S.  Open',f6.3,', close',f6.3,' seconds')
124   format('     RANDOM READS  :'
     *      ,i8,' blocks of',i8
     *      ,' bytes read in',f7.2,' S, at',f9.1
     *      ,' KB/S.  Open',f6.3,', close',f6.3,' seconds')
126   format('     RANDOM UPDATES:'
     *      ,i8,' blocks of',i8
     *      ,' bytes updt in',f7.2,' S, at',f9.1
     *      ,' KB/S.  Open',f6.3,', close',f6.3,' seconds')
128   format('         DD WRITES :'
     *      ,i8,' blocks of',i8
     *      ,' bytes writ in',f7.2,' S, at',f9.1
     *      ,' KB/S.  Open',f6.3,', close',f6.3,' seconds')
130   format('         DD READS  :'
     *      ,i8,' blocks of',i8
     *      ,' bytes read in',f7.2,' S, at',f9.1
     *      ,' KB/S.  Open',f6.3,', close',f6.3,' seconds')
210   format(' SEQUENTIAL WRITES ',/)
212   format(' SEQUENTIAL READS  ',/)
214   format('     RANDOM READS  ',/)
216   format('     RANDOM UPDATES',/)
218   format('         DD WRITES ',/)
220   format('         DD READS  ',/)
c
      end
