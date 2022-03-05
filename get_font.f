      subroutine get_font
c
c      Read font definitions
c
      use bmp_comms
c
      character(1)fg,bg
c
      open(43,file='font_def.matrix',status='unknown',err=98)
      read(43,105,end=97,err=99)l1,l2
      allocate(font_def(l1:l2,0:12))
      fg=char(0)
      bg=char(255)
      do n=0,12
      read(43,104,end=97,err=99)(font_def(mx,n),mx=l1,l2)
         do mx=l1,l2
            if(font_def(mx,n).eq.'X'
     *        )then
                   font_def(mx,n)=fg
               else
                   font_def(mx,n)=bg
            endif
         enddo
      enddo
      close(43)
c
      return
c
 97   continue
      write(0,100)
      stop
c
 98   continue
      write(0,101)
      stop
c
 99   continue
      write(0,102)
      stop
c
100   format('ERROR - EOF IN FONT_DEF_MATRIX IN get_font')
101   format('ERROR OPENING FONT_DEF_MATRIX IN get_font')
102   format('ERROR READING FONT_DEF_MATRIX IN get_font')
104   format(2048a1)
105   format(2i8)
c
      end
