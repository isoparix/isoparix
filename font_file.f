      program font_file
c
c      uses banner to generate file to generate font matrices...
c
c      THIS PROGRAM IS TO RUN UNDER CYGWIN!!!
C
      character (1) charline(0:15)
      character (1), allocatable :: font_def(:,:)
      character (8) filename
c
      n1=35
      n2=126
c
      open(8,file='banner_matrix.bat',status='unknown')
      write(8,106)
      do i=n1,n2
         write(filename,101)i
         write(8,100)char(i),filename
      enddo
      write(8,108)
      close(8)
c
      call system('./banner_matrix.bat')
c
      l1=n1*8
      l2=n2*8+7
      allocate(font_def(l1:l2,13))
      font_def='.'
c
      do i=n1,n2
         write(filename,101)i
c        write(*,109)filename
         open(3,file=filename,err=99)
         do j=1,13
            read(3,107,iostat=irc)charline
            do k=0,7    ! Leading Q is to allow things like minus sign..
               font_def((i*8)+k,j)=charline(k+8)
            enddo
         enddo
         close(3)
      enddo
c
      open(10,file='font_def.matrix',status='unknown')
      write(10,105)l1,l2
      do n=1,13
         write(10,104)(font_def(mx,n),mx=l1,l2)
      enddo
      close(10)
c
      stop
c
 99   continue
      write(*,102)filename
      stop
c
100   format('call banner "Q',a1,'" > ',a8)
101   format(i4.4,'.mat')
102   format('ERROR IN FONT_FILE - opening file ',a8)
103   format(8a1)
104   format(2048a1)
105   format(2i8)
106   format('@echo off')
107   format(16a1)
108   format('dir *.mat',/'exit')
109   format(a8)
c
      end
