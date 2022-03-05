      subroutine da_blksize(n,nblks,nsize)
c
c      Read block size and compute number of blocks
c
      integer (8) n,nblks
c
      read(*,*,end=99)nsize
      if(nsize.le.0
     *  )then
             stop
      endif
c
      nblks=n/nsize
      if(nblks.le.0
     *  )then
             return
      endif
c
      return
c
 99   stop
c
      end
