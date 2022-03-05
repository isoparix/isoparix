      subroutine greymap
c
c      Create grey-scale - results in rgbquad
c
      use bmp_comms
c  
      implicit real(8) (a-h,o-z)      
c
      character(1)rgb(0:1023)
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
      return
c      
      end
