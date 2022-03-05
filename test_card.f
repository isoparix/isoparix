      program test_card
c
c      Draws a 7" x 5" test card
c
      use bmp_comms
c
      implicit real *8 (a-h,o-z)
c
      call greymap
c
      write(*,103)
      read(*,*)ndpi
      write(bmname,101)ndpi
c
      iybm=ndpi*5
      ixbm=ndpi*7
      ixmax=ixbm-1
      iymax=iybm-1
      allocate(canvas(0:ixmax,0:iymax))
      canvas=char(255)
c
      kdiv=iybm/100
      nmax=100*kdiv
      nmed= 10*kdiv
      nmin=    kdiv
c
      canvas(0:ixmax     ,0:iymax:nmax)=char(0)
      canvas(0:ixmax     ,1:iymax:nmed)=char(0)
      canvas(0:ixmax     ,2:iymax:nmin)=char(0)
      canvas(0:ixmax     ,3:iymax:nmed)=char(0)
      canvas(0:ixmax     ,4:iymax:nmax)=char(0)
c
      canvas(0:ixmax:nmax,0:iymax     )=char(0)
      canvas(1:ixmax:nmed,0:iymax     )=char(0)
      canvas(2:ixmax:nmin,0:iymax     )=char(0)
      canvas(3:ixmax:nmed,0:iymax     )=char(0)
      canvas(4:ixmax:nmax,0:iymax     )=char(0)
c
      call array2bmp(ixbm,iybm,canvas)
c
      stop
c
100   format(2i6,5f8.2)
101   format('TestCard',i4.4)
102   format(i4,4f8.4,'  ',a)
103   format('DPI') 
104   format(i2.2) 
c
      end
