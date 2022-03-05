      program resolution
c      
c      Generates bit map according to aperture of telescope
c
      use bmp_comms
c  
      implicit real(8) (a-h,o-z)      
      real(8) lamda
c
      character(1),allocatable,dimension (:,:) :: picture
      integer,allocatable,dimension (:,:) :: inp_array
      real(4),allocatable,dimension (:,:) :: gmask,blurred
     *                                      ,synth_array,blur_pic
c
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
c      Create grey-scale
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
      lamda=550.0E-09  ! wavelength of 'green'
      arc_sec2rad=2.0*355.0/(113.0*360.0*3600.0)          ! Arc-seconds
c
      nblock=8
      nwidth=64
      nside=nblock*nwidth
      x_psi=15.0*arc_sec2rad
      aperture=75.0E-03
c
      ta=lamda*float(nside)/(aperture*x_psi)
      tb=-2.5/(ta**2)
      ntheta=0.5+(1.5*ta)
      write(*,100)x_psi,lamda,aperture,ta,ntheta
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
      allocate(picture    (nside+(2*ntheta)+1,nside+(2*ntheta)+1))
      allocate(synth_array(-ntheta:nside+ntheta,-ntheta:nside+ntheta))
c
c      Populate synth_array
c
      nxcen=nside/2
      nycen=nside/2
      rmax2=nxcen**2
      rmin2=0.7*rmax2
c
      synth_array=0
      it=nwidth-1
      do iy=1,nblock-1,2
         nya=1+((iy-1)*nwidth)
         nyb=nya+it
         do ix=1,nblock-1,2
            nxa=1+((ix-1)*nwidth)
            nxb=nxa+it
            synth_array(nxa:nxb,nya:nyb)=255
            nxa=nxb+1
         enddo
         nya=nyb+1
      enddo
c
      do iy=1,nside
         dy2=(iy-nycen)**2
         do ix=1,nside
            dx2=(ix-nxcen)**2
            r2=dx2+dy2
            if(r2>rmin2
     *        )then
                   if(r2.gt.rmax2
     *               )then              
                          synth_array(ix,iy)=0.0
                      else
                          synth_array(ix,iy)=255.0
                   endif 
            endif 
         enddo
      enddo
c
      bmname='master'
      picture=char(int(synth_array))
      call array2bmp(nside+(2*ntheta)+1,nside+(2*ntheta)+1,picture)
c
c      Blur the synth_array
c
      allocate(blur_pic(-ntheta:nside+ntheta,-ntheta:nside+ntheta))
      allocate(blurred (-ntheta:ntheta,-ntheta:ntheta))      
      blur_pic=0.
c
      do ny=1,nside
         do nx=1,nside
            blurred=gmask*synth_array(nx,ny)
            blur_pic(nx-ntheta:nx+ntheta,ny-ntheta:ny+ntheta)
     *     =blur_pic(nx-ntheta:nx+ntheta,ny-ntheta:ny+ntheta)+blurred
         enddo
      enddo
c
      a=maxval(blur_pic)
      blur_pic=255.0*blur_pic/a
c
      bmname='blur_pic'
      picture=char(int(blur_pic))
      call array2bmp(nside+(2*ntheta)+1,nside+(2*ntheta)+1,picture)
c
      stop
c
100   format('    x_psi=',e12.5
     *     ,/'    lamda=',e12.5
     *     ,/' aperture=',e12.5
     *     ,/'       ta=',e12.5
     *     ,/'   ntheta=',i6)
101   format(50f8.3)         
c
      end
