      subroutine da_init(mounter,filesystem,filename,n,mountdata)
c
c      Read parameters and assign a filename
c
      integer (8) n
c
      character (50) filename
      character (50) filesystem
      character (50) mountdata
      character (10) timename
c
      logical mounter
c
      real (8) tname
c
c      Generate a time-based name
c
      call tim(tname)
      write(timename,100)int(10000.*tname)
c
c      Read the mount choice, filesystem name, file size in bytes
c      and block size in bytes
c
      read(*,101)mounter
      read(*,102)filesystem
      read(*,*)n
c
      if(n.le.0
     *  )then
             return
      endif
c
      if(mounter
     *  )then
c
c      Find the real filesystem
c
             call mountpt(filesystem,mountdata,irc)
      endif
c 
      filename=trim(filesystem)//'/tmp'//timename
c
      return
c
100   format(i10.10)
101   format(l1)
102   format(a50)
      end
