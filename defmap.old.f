      subroutine defmap(iret)
c
c      Defines a colour map for Windows bitmap 
c
      use bmp_comms
c
      character (1) rgb(1024)
      equivalence(rgb,rgbq)
      character (1024) rgbq
c
c      Read in the details of the colour map
c
c0.5 0.5 0.5      ;Distance through sin**2(x) wave cycle (x/pi) at start 
c0.0 0.0 0.0      ;Minimum level of this colour (0-1)
c  1   2  3       ;Number of quarter-periods of this colour
      phired  =-999.0
        ared  =-998.0
        ired  =-997.0
      phigreen=-999.0
        agreen=-998.0
        igreen=-997.0
      phiblue =-999.0
        ablue =-998.0
        iblue =-997.0
c
      read(7,*,err=900,end=901)phired,phigreen,phiblue
      read(7,*,err=900,end=901)  ared,  agreen,  ablue
      read(7,*,err=900,end=901)  ired,  igreen,  iblue
c
      pred  =ired  
      pgreen=igreen
      pblue =iblue 
c
      pi=355./113.
      n_palette=254
c
      a_palette=n_palette
      pi_rnp=pi/a_palette
c
      phired  =phired  *pi
      phigreen=phigreen*pi
      phiblue =phiblue *pi
c
c      Generate the colour map for bitmaps...
c
      pr=float(ired  )*pi_rnp
      pg=float(igreen)*pi_rnp
      pb=float(iblue )*pi_rnp
c
c      Make our own colour mapping, instead of the X-server doing it...
c
      rgb=char(0)
      do k=1,n_palette
c
         mx=4*k
c
         ar=(1.-ared  )*(sin(.25*pr*float(k)+phired  )**2)+ared
         ag=(1.-agreen)*(sin(.25*pg*float(k)+phigreen)**2)+agreen
         ab=(1.-ablue )*(sin(.25*pb*float(k)+phiblue )**2)+ablue
c
         rgb(mx+3)=char(int((a_palette*ar)+.5))
         rgb(mx+2)=char(int((a_palette*ag)+.5))
         rgb(mx+1)=char(int((a_palette*ab)+.5))
c
      enddo
c
c      Set White Pixel
c
      rgb(1021:1023)=char(255)
c
      rgbquad=rgbq
c
      open(30,file='bitmap.rgb',form='formatted',status='unknown')
      write(30,100)ichar(rgb)
      call isoflush(30)
      close(30)
c
      open(50,file='ascii.rgb',form='formatted',status='unknown')
      do k=0,255
         mx=4*k
         write(50,103)k,(ichar(rgb(1+mx+my)),my=0,2)
      enddo
      call flush(50)
      close(50)
c
      iret=0
      return
c
900   continue
      iret=1
      write(0,101)
      return 
c
901   continue
      iret=2
      write(0,102)
      write(0,*)phired,phigreen,phiblue
      write(0,*)  ared,  agreen,  ablue
      write(0,*)  ired,  igreen,  iblue
      return 
c
100   format(4(z4,1x))
101   format('##### ERROR IN DEFMAP: Error reading colour-map file')
102   format('##### ERROR IN DEFMAP: Premature end of colour-map file')
103   format(4i4)
126   format(2(3f4.1,/),3i4)
      end
