      program font_read
c
c      Reads font definitions
c
      character (1) charline(0:15)
      character (1), allocatable :: font_def(:,:)
      character (8) filename
c
      open(10,file='font_def.matrix',status='unknown',err=99)
      read(10,105)l1,l2
      allocate(font_def(l1:l2,13))
      font_def=' '
      write(*,*)l1,l2
      do n=1,13
         read(10,104)(font_def(mx,n),mx=l1,l2)
      enddo
      do n=1,13
         write(*,104)(font_def(mx,n),mx=65*8,(81*8)-1)
      enddo
      close(10)
c
  1   continue
c
      read(*,*)nchar
      if(nchar.lt.0
     *  )then
             stop
         else
             write(*,103)
     *       ((font_def(mx,my),mx=nchar*8,(nchar*8+7)),my=1,13)
      endif
c
      go to  1
c
 99   continue
      write(*,102)
      stop
c
100   format('call banner "Q',a1,'" > ',a8)
101   format(i4.4,'.mat')
102   format('ERROR READING FONT_DEF_MATRIX IN FONT_READ')
103   format(8a1)
104   format(2048a1)
105   format(2i8)
106   format('@echo off')
107   format(16a1)
c
      end
