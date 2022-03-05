      program label_bmp
c
c      Tests label_maker
c
      use bmp_comms
c
      parameter(ixbm=1280,iybm=1024)
      character (1) inp_array(ixbm,iybm)
c
      character (28) date_txt
c
      character (37) label_txt(11)
      character (1) label_chars(0:36,0:10)
      equivalence(label_txt,label_chars)
c
      character (20) input_txt
      character (1) small_txt(0:19)
      equivalence(input_txt,small_txt)
c
      ixm=ixbm
      iym=iybm
      call defmap(254,7,iret)
c
      write(*,100)
      read(*,*)mag
      if(mag.le.0)stop
c
      write(*,101)
      read(*,*)lx,ly,input_txt
      write(*,*)input_txt
      write(*,*)small_txt
      bmname='Text_box'
      inp_array=char(64)
c
      mlc=20
c     call text_writer(mag,lx,ly,ixm,iym,small_txt,mlc)
c     call array2bmp(ixbm,iybm,inp_array)
c
      inp_array=char(64)
c
      bmname='Test_label'
c
      inp_array=char(64)
      rewind(1)
      read(1,200)xcen,ycen,deltay,cra,cia
      open(43,file='date.txt',status='old')
      read(43,201)date_txt
      close(43)
c
      write(label_txt,144)xcen,ycen,deltay,cra,cia
     *                          ,ixm,iym,mag,date_txt
      lx=8*mag
      ly=13*mag
c
      call label_maker(mag,lx,ly,ixm,iym,inp_array,label_chars,36,10)
      call array2bmp(ixbm,iybm,inp_array)
c
      stop
c
100   format('Magnification? ',$)
101   format('Centre X and Y (0 0 top left, as X_windows), and text? '
     *      ,$)
144   format(
     *        '+=========== ISOPARIX ==============+'
     *      ,/'* Centre X:',e24.16,' *'
     *      ,/'* Centre Y:',e24.16,' *'
     *      ,/'*  Delta Y:',e11.3,13x,' *'
     *      ,/'*  Const X:',e24.16,' *'
     *      ,/'*  Const Y:',e24.16,' *'
     *      ,/'*  Picture:',2i6,',  Label:',i2,'x *'
     *      ,/'*                                   *'
     *      ,/'*       Copyright: John Watts       *'
     *      ,/'*    ',a28,'   *'
     *       /'+=========== ISOPARIX ==============+'
     *      )
200   format(5(e24.17,/))
201   format(a28)
c
      end
