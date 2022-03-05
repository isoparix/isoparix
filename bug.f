      program label_bmp
c
c      Reads font definitions
c
      character (1) label_txt(0:39,0:14)
      character (1), allocatable :: font_def(:,:)
c
      open(43,file='font_def.matrix',status='unknown',err=99)
      read(43,105)l1,l2
      allocate(font_def(l1:l2,13))
      font_def=' '
      do n=1,13
         read(43,104)(font_def(mx,n),mx=l1,l2)
      enddo
      close(43)
c
      label_txt='#'
      call date_and_time(datea,dateb)
      write(label_txt,144)xcen,ycen,deltay,cra,cia,ixm,iym,datea,dateb
      write(*,103)((label_txt(mx,my),mx=0,40),my=0,15)
c
      stop
c
 99   continue
      write(*,102)
      stop
c
102   format('ERROR READING FONT_DEF_MATRIX IN label_bmp')
103   format(15(40a1))
104   format(2048a1)
105   format(2i8)
144   format(
     *       /'+=========== ISOPARIX ==============+'
     *      ,/'* Centre X:',e24.16,' *'
     *      ,/'* Centre Y:',e24.16,' *'
     *      ,/'*  Delta Y:',e11.3,13x,' *'
     *      ,/'*  Const X:',e24.16,' *'
     *      ,/'*  Const Y:',e24.16,' *'
     *      ,/'*  Picture:',2i6,12x,' *'
     *      ,/'*                                   *'
     *      ,/'*       Copyright: John Watts       *'
     *      ,/'*        ',2a10,'       *'
     *       /'+=========== ISOPARIX ==============+'
     *      ,/)
      end
c
      end
