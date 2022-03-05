      program slide_rename
c
c      Resequences file names in a directory with directory name as root
c
c9/04/2004  21:53    <DIR>          gbma
c7/01/1991  12:51             62281 FLTREE.EXE
c
      character (120) filename
      character (120) slidename
      character (1) answer,txt(120)
c
      equivalence(slidename,txt)
c
c      Prepare a default slide name
c
      read(9,109,iostat=ios)txt
c
c      Find the last separating / or \
c
      do i=1,120
         if(txt(i).eq.'/'.or.
     *      txt(i).eq.'\'
     *     )then
                nsep=i
         endif
      enddo
c
c      Remove non-alphanumerics from the slidename
c
      n=1
      do i=1+nsep,120
         k=ichar(txt(i))
         if((k.ge.48.and.k.le.57).or.	! 0-9
     *      (k.ge.65.and.k.le.90).or.	! A-Z
     *      (k.ge.97.and.k.le.122).or.  ! a-z
     *      (txt(i).eq.'_')
     *     )then
                txt(n)=txt(i)
                n=n+1
         endif
      enddo
      txt(n:120)=' '
c
c     Get a slide family name
c
      write(*,103)trim(adjustl(slidename))
      answer=' '
      write(*,111)
      read(*,100)answer
      if(answer.ne.'y'.and.answer.ne.'Y'
     *  )then
             write(*,112)
             read(*,100)slidename
      endif
c
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
103   format('Default slide family name is: ',a)
104   format('No changes made',/)
105   format('@echo off')
106   format(/'Do you want to rename these files like this (Y/y)?')
107   format('cmd /c border.bat ',a,'_',i4.4)
108   format(/'Do you want to crop these files like this (Y/y)?')
109   format(80a1)
110   format(a,' ',2i4)
111   format('Do you want to keep this default family name?')
112   format('Please enter new family name for slides')
c
      end
