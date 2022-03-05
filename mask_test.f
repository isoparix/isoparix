      program mask_test
c
c      Explores unsharp masking
c
      use bmp_comms
      use ip_comms
c
      implicit real(8) (a-h,o-z)
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      sigma_blur=2.0
      ixdim=10
      iydim=10
      allocate(  ipic(-50:ixdim+50,-50:iydim+50))
      allocate(picdata(0:ixdim,0:iydim))
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
      do iy=0,iydim
         do ix=0,ixdim
            x=sqrt(float(((ix-6)**2)+((iy-6)**2)))
            radius=255.0*exp((-x**2)/8.0)
            ipic(ix,iy)=int(.5+radius)
         enddo
      enddo
     
c
      write(*,102)ipic(0:ixdim,0:iydim)
      bmname='original'
      picdata=char(ipic(0:ixdim,0:iydim))
      call array2bmp(ixdim+1,iydim+1,picdata)
c
      bmname='orig_sharp'
      call ip_sharpen('gaussian')
      write(*,102)ipic(0:ixdim,0:iydim)
      picdata=char(ipic(0:ixdim,0:iydim))
      call array2bmp(ixdim+1,iydim+1,picdata)
c
      stop
c
100   format(11(/,11f10.3))      
101   format(/)
102   format(11(/,11i10))      
c
      end
