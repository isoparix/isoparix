      subroutine label_maker(mag,nxlab,nylab,ixbm,iybm,inp_array
     *                      ,label_chars,labchars_x,labchars_y)
c
c      Reads font definitions
c
c      mag      =magnification over original font
c      n*lab    =x and y co-ordinates (X-windows, 0,0=top left) of label
c      i*bm     =array width and height
c      inp_array=character (1) 2D array of input data
c
      character (1)  label_chars(0:labchars_x,0:labchars_y)
      character (1) inp_array(ixbm,iybm),fg,bg
c
      character (1), allocatable :: font_def(:,:)
c
      open(43,file='font_def.matrix',status='unknown',err=99)
      read(43,105)l1,l2
      allocate(font_def(l1:l2,0:12))
c
      fg=char(0)
      bg=char(255)
      do n=0,12
         read(43,104)(font_def(mx,n),mx=l1,l2)
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
      do j=0,labchars_y
         do i=0,labchars_x
            index_font=8*ichar(label_chars(i,j))
            index_array_x=nxlab+( 8*mag*i)
            index_array_y=nxlab+(13*mag*j)
            if(index_font.ge.l1.and.
     *         index_font.le.l2
     *        )then
                   do m=0,7
                      do n=0,12
                         mia=index_array_x+(m*mag)
                         mib=index_array_x+(m*mag)+mag
                         nia=index_array_y+(n*mag)
                         nib=index_array_y+(n*mag)+mag
                         inp_array(mia:mib,nia:nib)
     *                      =font_def(index_font+m,n)
                      enddo
                   enddo
               else
                   inp_array(index_array_x:index_array_x+( 8*mag)-1
     *                      ,index_array_y:index_array_y+(13*mag)-1)=bg
             endif
         enddo
      enddo
c
c      Paste a colour bar on the bitmap
c
      m=mag-1
c
      ja=nxlab+(8*mag)
      jb=nxlab+(8*mag*3)
c
      jc=(ja+jb)/2
      jd=(ja+jc)/2
      je=(jb+jc)/2
c
      ka=nylab+(13*mag*6)+(9*mag)
      kb=ka+(12*mag)
c
      la=jb
      do i=0,255
         lb=la+m
         inp_array(la:lb,ka:kb)=char(i)
         la=lb+1
      enddo
c
c      Paste calibration on the bitmap
c
      do n=1,3
c
         kcya=(13*mag*(n-1))+ka
         kcyb=(12*mag*(n-1))+kb
         kcyc=(kcya+kcyb)/2
c
c      Outer box 
c
         inp_array(ja:jb,kcya)=char(0)
         inp_array(ja:jb,kcyb)=char(0)
         inp_array(ja,kcya:kcyb)=char(0)
         inp_array(jb,kcya:kcyb)=char(0)
c
c      Cross-hairs
c
         inp_array(ja:jb,kcyc)=char(0)
         inp_array(jc,kcya:kcyb)=char(0)
c
         kc=(kcya+kcyb)/2
         kd=(kcya+kc)/2
         ke=(kcyb+kc)/2
c
         do nx=jd,je,2*n
            inp_array(nx:nx+n-1,kd:ke)=char(200)
         enddo
c
         do ny=kd,ke,2*n
            inp_array(jd:je,ny:ny+n-1)=char(200)
         enddo
c
      enddo
c
      return
c
 98   continue
      write(0,107)
      return
c
 99   continue
      write(*,102)
      return
c
100   format(15(40a1,/))
101   format(40(15a1,/))
102   format('ERROR READING FONT_DEF_MATRIX IN label_bmp')
103   format(a40)
104   format(2048a1)
105   format(2i8)
106   format(a28)
107   format('ERROR OPENING DATE.TXT IN LABEL_BMP')
c
      end
