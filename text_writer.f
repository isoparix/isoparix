      subroutine text_writer(mag,nxlab,nylab,ixbm,iybm
     *                      ,label_chars,labchars_x)
c
c      Places 8x13 font characters into bit-map canvas, centred on 
c      nxlab, nylab
c
c
c
c      mag      =magnification over original font
c      n*lab    =x and y central co-ordinates (0,0=top left) of label
c      i*bm     =array width and height
c
      use bmp_comms
c
      character (1)label_chars(0:labchars_x)
c
         write(8,107)mag,nxlab,nylab,ixbm,iybm,labchars_x
     *              ,(label_chars(mx),mx=0,labchars_x)
c
c      How many characters in the text string?
c
      mx=labchars_x
      do i=labchars_x,0,-1
         if(label_chars(i).eq." "
     *     )then
                mx=mx-1
            else
                exit
         endif
      enddo
      n_labchars_x=mx
         write(8,108)mag,nxlab,nylab,ixbm,iybm,n_labchars_x
     *              ,(label_chars(mx),mx=0,n_labchars_x)
c
c      Centre the text
c
         lxlab=nxlab-(((1+n_labchars_x)*8*mag)/2)
         lylab=nylab-((13*mag)/2)
c
         do i=0,n_labchars_x
            index_font=8*ichar(label_chars(i))
            write(8,109)index_font,char(index_font/8),l1,l2
            index_array_x=lxlab+( 8*mag*i)
            index_array_y=lylab
c
                   do m=0,7
                      do n=0,12
                         mia=index_array_x+(m*mag)
                         mib=index_array_x+(m*mag)+mag
                         nia=index_array_y+(n*mag)
                         nib=index_array_y+(n*mag)+mag
                         canvas(mia:mib,nia:nib)
     *                        =font_def(index_font+m,n)
                      enddo
                   enddo
         enddo
c
      return
c
107   format('TEXT_WRITER(A): Mag=',i3,', lxlab=',i5,', lylab=',i5
     *      ,', ixbm=',i5,', iybm=',i5
     *      ,', n_labchars_x=',i8
     *     ,/'                label_chars=>',40a1,'<')
c
108   format('TEXT_WRITER(B): Mag=',i3,', lxlab=',i5,', lylab=',i5
     *      ,', ixbm=',i5,', iybm=',i5
     *      ,', n_labchars_x=',i3
     *     ,/'                label_chars=>',40a1,'<')
109   format('TEXT_WRITER: index_font=',i5,', character=',a2
     *      ,', l1=',i8,', l2=',i8)
c
      end
