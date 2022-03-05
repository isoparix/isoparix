      program resolution
c      
c      Generates bit map according to aperture of telescope
c
      use bmp_comms
c  
      implicit real(8) (a-h,o-z)      
      real(8) lamda
      logical synthetic
      character(20)image_name
      character (4) pictype
c
      character(1),allocatable,dimension (:,:) :: picture,image_array
      real(4),allocatable,dimension (:,:) :: gmask,blurred
     *                                      ,blur_pic
c
      call greymap      !  Generate a grey-scale for bitmap
c
      lamda=550.0E-09  ! wavelength of 'green'
      arc_sec2rad=2.0*355.0/(113.0*360.0*3600.0)          ! Arc-seconds
c
      call picdims(pictype,nxside,nyside)   ! How big is it?  What type?
c
      open(1,file='pic.dat',form='unformatted'
     *       ,access='stream',status='old')
c
      allocate(image_array(1:nxside,1:nyside))      
      read(1,iostat=irc,err=98)image_array
c
      write(*,102)
      read(*,*)x_psi
      x_psi=x_psi*arc_sec2rad  ! X-width of image
      write(*,103)
      read(*,*)aperture
      aperture=0.001*aperture ! Telescope aperture
c
      ta=lamda*float(nxside)/(aperture*x_psi)
      tb=-2.5/(ta**2)
      ntheta=0.5+(1.5*ta)
      write(*,100)x_psi,aperture,nxside,nyside,ntheta,pictype
      allocate(gmask  (-ntheta:ntheta,-ntheta:ntheta))      
c
c      Create Gaussion mask
c
      do ny=-ntheta,ntheta
         ny2=ny*ny
         do nx=-ntheta,ntheta
            gmask(nx,ny)=exp(tb*float(ny2+(nx*nx)))
         enddo
c        write(*,101)(gmask(mx,ny),mx=-ntheta,ntheta)
      enddo
c
      allocate(picture    (nxside+(2*ntheta)+1,nyside+(2*ntheta)+1))
c
      bmname='master'
      call array2bmp(nxside,nyside,image_array)
c
c      Blur the image_array
c
      allocate(blur_pic(-ntheta:nxside+ntheta,-ntheta:nyside+ntheta))
      allocate(blurred (-ntheta:ntheta,-ntheta:ntheta))      
      blur_pic=0.
c
      nya=1-ntheta
      nyb=1+ntheta
      do ny=1,nyside
         nxa=1-ntheta
         nxb=1+ntheta
         do nx=1,nxside
            blurred=gmask*float(ichar(image_array(nx,ny)))
            blur_pic(nxa:nxb,nya:nyb)=blur_pic(nxa:nxb,nya:nyb)+blurred
            nxa=nxa+1
            nxb=nxb+1
         enddo
         nya=nya+1
         nyb=nyb+1
      enddo
c
      a=maxval(blur_pic)
      blur_pic=255.0*blur_pic/a
c
      bmname='blur_pic'
      picture=char(int(blur_pic))
      call array2bmp(nxside+(2*ntheta)+1,nyside+(2*ntheta)+1,picture)
c
 98   continue      
c
      stop
c
100   format('     x_psi=',e12.5
     *     ,/'  aperture=',e12.5
     *     ,/'    nxside=',i6
     *     ,/'    nyside=',i6
     *     ,/'    ntheta=',i6
     *     ,/'image type=',a4)
101   format(50f8.3)         
102   format('Angular width of image, in arc-seconds?')      
103   format('Aperture of telescope, in millimetres?')      
104   format('Full name of image to be diffracted?')      
c
      end
