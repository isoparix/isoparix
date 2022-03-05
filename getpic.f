      subroutine getpic
c
      use bmp_comms
      use ip_comms
c
c      Gets jpg pixels
c
      character (4) pictype
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
      call picdims(pictype,ixdim,iydim)   ! How big is it?  What type?
c
      npixels=ixdim*iydim
      write(*,110)pictype,ixdim,iydim,npixels
c
      allocate(picdata   (0:ixdim-1,0:iydim-1))
      allocate(ipic      (0:ixdim-1,0:iydim-1))
c
      picdata=char(0)
      ix=0
      iy=0
c
      open(1,file='pic.dat',form='unformatted',err=99
     *       ,access='stream',status='old')
c
      read(1,iostat=irc,err=98)picdata
c
      bmname='original'
      call array2bmp(ixdim,iydim,picdata)
c
      ipic=0
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ipic(ix,iy)=ichar(picdata(ix,iy))
         enddo
      enddo
c
      return
c
 98   continue
      write(0,100)irc
      stop
c
 99   continue
      write(0,101)irc
      stop
c
100   format('GETPIC: Error reading picture data - error',i6)
101   format('GETPIC: Error opening picture file')
110   format('Picture type is >',a4,'<.  Dimensions are '
     *       ,i0,'x',i0,' - ',i10,' pixels')
200   format('IP_MASTER OK TO HERE...')
c
      end
