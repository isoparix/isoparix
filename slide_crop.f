      program slide_crop
c
c      Resequences and crops file names in a directory
c
c9/04/2004  21:53    <DIR>          gbma
c7/01/1991  12:51             62281 FLTREE.EXE
c
      character (120) filename
      character (120) slidename
      character (500) command
      character (1) answer
c
c     Get a slide family name
c
      write(*,103)
      read(*,100)slidename
      n=0
      open( 8,file='rename_jpg.bat',status='unknown')
      open(10,file='crop_all.bat',status='unknown')
      write(8,105)
  1   continue
      read (3,100,err=3,end=2)filename
      if(trim(adjustl(filename)).eq.'fort.3')go to 1
      n=n+1
      write( 8,101)trim(adjustl(filename))
     *            ,trim(adjustl(slidename)),n
      write(10,107)trim(adjustl(slidename)),n
      go to 1
c
  3   continue
      write(*,102)
      stop
c
  2   continue
c
      call system('type rename_jpg.bat')
      answer=' '
      write(*,106)
      read(*,100)answer
      if(answer.eq.'y'.or.answer.eq.'Y'
     *  )then
             call system('rename_jpg.bat')
         else
             write(*,104)
             stop
      endif
c
      call system('type crop_all.bat')
      answer=' '
      write(*,108)
      read(*,100)answer
      if(answer.eq.'y'.or.answer.eq.'Y'
     *  )then
             call system('crop_all.bat')
         else
             write(*,104)
      endif
c
      stop
c
100   format(a)
101   format('rename "',a,'" ',a,'_',i4.4,'.jpg')
102   format('Error in read - program ends')
103   format('Enter new slide family name')
104   format('No changes made',/)
105   format('@echo off')
106   format(/'Do you want to rename these files like this (Y/y)?')
107   format('cmd /c border.bat ',a,'_',i4.4)
108   format(/'Do you want to crop these files like this (Y/y)?')
c
      end
