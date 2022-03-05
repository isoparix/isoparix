      program ip_greyscale
c
c      Produces grey-scale test pattern
c
      use bmp_comms

      character(1)rgb(0:1023),greydata(0:767,0:511)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      do n=0,255
         m=n*4
         rgb(m  )=char(n)
         rgb(m+1)=char(n)
         rgb(m+2)=char(n)
         rgb(m+3)=char(0)
      enddo
      rgbquad=rgbq
c
c      Do background first
c
      do ix=0,255
         jxa=ix*3
         greydata(jxa:jxa+2,0:511)=char(ix)
      enddo
c
c      Now overwrite with 'displaced' lines
c
      iya=0
      do iy=15,495,16
         iya=iya+1
         do ix=0,255
            jxa=ix*3
            greydata(jxa:jxa+2,iy:iy+4)=char(ix+iya)
         enddo
      enddo
c
c      Overwrite at intervals of 10
c
      do ix=0,255,10
         jxa=ix*3
         greydata(jxa,0:511)=char(100)
      enddo
c
c      Now write out the grey-scale test card
c
      bmname='greyscale'
      call array2bmp(768,512,greydata)
      stop
      end
