      subroutine mountpt(filesystem,filemount,irc)
c
c      Read the real filename...
c
      character (50) filesystem
      character (50) filemount
      character (50) mountdata
      character (10) timename
      character (1) txt(50)
      equivalence (txt,mountdata)
c
      real (8) tname
c
      call tim(tname)
      write(timename,103)int(10000.*tname)
      filemount=timename//'_fs.mnt'
      call system('df '//trim(filesystem)//' > '//trim(filemount),irc)
c
      if(irc.ne.0
     *  )then
             write(0,101)filesystem
             go to 98
      endif
c
      open(31,file=trim(filemount),status='old',err=97)
      read(31,*,err=98)mountdata
      read(31,100,err=98)mountdata
      close(31)
      call system('rm '//trim(filemount),irc)
c
      if(irc.ne.0
     *  )then
             write(0,104)filesystem
      endif
c
c      Parse the line
c
      do n=1,50
         if(txt(n).eq.' '
     *     )then
                do m=n,50
                   txt(m)=' '
                enddo
                exit
         endif
      enddo
      filemount=mountdata
c
      return
c
 97   continue
      write(0,102)filemount
c
 98   continue
      write(0,119)filesystem
      stop
c
100   format(a50)
101   format('Could not df ',a)
102   format('Could not open ',a)
103   format(i10.10)
104   format('***** WARNING in MOUNTPT - Could not remove file',a)
119   format('***** ERROR in MOUNTPT - could not resolve mount point'
     *      ,' for filesystem ',a)
c
      end
