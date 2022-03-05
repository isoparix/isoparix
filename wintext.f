      program wintext
c
c	Added to check out CVS
c
c      Reads a file and displays contents in a window...
c
      parameter(length_text=500)
c
      character (1) blank
      character(20)prog_name
      character(length_text)file_name,title,textin
      character(length_text),allocatable,dimension (:) :: text
c
      integer font_width
      integer font_height
      integer font_def
c
      margs=2
      blank=' '
c
      nargs=iargc()
      call getarg(0,prog_name)
      if(nargs.ne.margs
     *  )then
             write(0,100)trim(prog_name),nargs,margs
             stop
      endif
c
c      How big is this display?
c
      call x11dispinfo(ixmax,iymax,nwhite,nblack,irc)
c
      call getarg(1,file_name)
      call getarg(2,title)
      title=trim(title)//'\0'
      open(3,file=trim(file_name),status='old',err=98)
      maxlen=0
      nlines=0
  1   continue
      read(3,101,iostat=ios,end=2)textin
c
      if(len_trim(textin).gt.maxlen
     *  )then
             maxlen=len_trim(textin)
      endif
      nlines=nlines+1
      go to 1     
c
  2   continue
c
      allocate (text(nlines))
c
c      Allocate the font-size
c
      ivx=(iymax-50)/nlines
c
      if(ivx.le.7
     *  )then
             font_width =5
             font_height=7
             font_def   =5
      endif
c
      if(ivx.eq.8
     *  )then
             font_width =5
             font_height=8
             font_def   =4
      endif
c
      if(ivx.eq.9
     *  )then
             font_width =6
             font_height=9
             font_def   =3
      endif
c
      if(ivx.ge.10.and.ivx.le.11
     *  )then
             font_width =6
             font_height=10
             font_def   =2
      endif
c
      if(ivx.eq.12
     *  )then
             font_width =6
             font_height=12
             font_def   =1
      endif
c
      if(ivx.gt.12
     *  )then
             ivx=13
             font_width =8
             font_height=13
             font_def   =0
      endif
c
c      Don't want too small a window...
c
      if(nlines.lt.20
     *  )then
             iym=400
             iy1=(iym-(nlines*font_height))/2
         else
             iym=(nlines*font_height)+3
             iy1=0
      endif
c 
      ixm=(2+maxlen)*font_width
c
c      Open up the display window
c
      write(8,106)ixm,iym,trim(title)
      call x11textwin(%val(ixm),%val(iym)
     *               ,trim(title))

      rewind(3)
      do nl=1,nlines
         read(3,101,iostat=ios,end=3)text(nl)
      enddo
      close(3)
c
  5   continue
      do nl=1,nlines
         iytxt=iy1+(nl*ivx)
         textin=text(nl)
         lentext=len_trim(textin)
         if(lentext.gt.0
     *     )then
                call x11textw(%val(font_width),%val(iytxt)
     *                       ,%ref(trim(textin))
     *                       ,%val(font_def),%val(lentext))
            else
                call x11textw(%val(font_width),%val(iytxt)
     *                       ,%ref(blank)
     *                       ,%val(font_def),%val(lentext))
         endif
      enddo
  3   continue
c
c
  4   continue
      call microsleep(100000)
      call x11mouse(nbut,mousex,mousey,idummx,idummy)
      if(nbut.eq.-10)go to 5 
      if(nbut.eq.-1)stop 
      go to 4
c
      stop
c
 98   continue
      write(0,103)trim(prog_name),trim(file_name)
      stop
c
100   format('***** ERROR in ',a,':',i6,' arguments - should have',i2
     *      ,' - a file_name and a title')
101   format(a)
102   format(2a)
103   format('***** ERROR in ',a,': Could not open file ',a)
104   format(i6,' lines read - max length=',i3,3i8)
105   format('WINTEXT: Display is',2i8)
106   format('WINTEXT: Opening window -',2i8,a)
c
      end
