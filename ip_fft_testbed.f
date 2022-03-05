      program ip_fft
c
      use bmp_comms
      use ip_comms
c
c      Gets jpg pixels, then:
c         - Stores the image data in a complex array cpicdata
c         - Creates the complex FFT of the image in c2df_image
c         - Generates and displays a bitmap of this inverse (=original)
c         - Generates a 2D gaussian in the same square array space
c           (with power_of_2 sides) as the inverse.
c         - Generates a complex FFT of the Gaussian in c2df_psf
c
c
      real(8)brighten,ta,tb
c
      logical distributed,bad_action
c
      character (4) pictype
      character (5) charix,chariy
      character (8) convolution_type
      character (20) char_thresh
      character(1),allocatable,dimension(:,:) :: rpic
      character(1)rgb(0:1023)
      character(1024)rgbq
      equivalence(rgb,rgbq)
c
      integer sizedim
      integer  (2),allocatable,dimension(:,:) :: cr2pic
c
      complex(8)  ,allocatable,dimension(:,:) :: cpicdata,c2df_image
     *                             ,cx,cy,kernel_psf,cpsf,c2df_psf
      complex(8)  ,allocatable,dimension(:)   :: w
      complex(8)cxa,cxb,cxc,cxd,cxe
c
      bad_action=.true.
c
      call greymap
c     call xxstatmap
c
      call getarg(1,char_thresh)
      read(char_thresh,*,end=97)nsteps
c
      call getarg(2,char_thresh)
      read(char_thresh,*,end=97)fringe
c
c     fringe: Level of Gaussian curve at centre of edges of square array
c
      call getarg(3,char_thresh)
      read(char_thresh,*,end=97)sigma_blur
c
c     Complex numbers ARE multiplied correctly...
c
      if(nsteps.eq.0   !  Complex number arithmetic demo
     *  )then
             bad_action=.false.
             write(*,*)'Enter two complex numbers as (12.00,-34.5)'
  1          continue
                read(*,*)cxa
                read(*,*)cxb
                cxc=cxa*cxb
                cxe=cxb*cxa
                cxd=cxa/cxb
                write(*,*)'A=',cxa,', B=',cxb
                write(*,*)'A*B=',cxc,', B*A=',cxe,', A/B=',cxd
                write(*,*)'A+B=',cxa+cxb,', A-B=',cxa-cxb
     *                   ,', 3*A=',3.0*cxa
             go to 1
      endif
c
c ##################################################################
c                           End of action 0
c ##################################################################
c
      nxmax=0
      nxmin=1000000
      nymax=0
      nymin=1000000
c
      if(nsteps.eq.1   !  Read from a JPEG file
     *  )then
             bad_action=.false.
             call picdims(pictype,ixdim,iydim)   ! How big is it?  What type?
             npixels=ixdim*iydim
             write(*,110)pictype,ixdim,iydim,npixels
c
             allocate(picdata   (0:ixdim-1,0:iydim-1))
             allocate(ipic      (0:ixdim-1,0:iydim-1))
             allocate(rpic      (0:ixdim-1,0:iydim-1))
c
             picdata=char(0)
c
             open(1,file='pic.dat',form='unformatted'
     *              ,access='stream',status='old')
c
             read(1,iostat=irc,err=98)picdata
             write(*,*)'Picture data read'
c
             ipic=0
             do iy=0,iydim-1
                do ix=0,ixdim-1
                   ipic(ix,iy)=ichar(picdata(ix,iy))
                enddo
             enddo
             write(*,*)'Picture data now in ipic'
c            deallocate(picdata)
      endif
c
c ##################################################################
c                           End of action 1
c ##################################################################
c
      if(nsteps.eq.2   !  Create a diagonal pattern
     *  )then
             bad_action=.false.
             ixdim=1024
             iydim=1024
             allocate(ipic (0:ixdim-1,0:iydim-1))
             allocate(rpic (0:ixdim-1,0:iydim-1))
             ipic(0:ixdim-1,  iydim-30:iydim-1)=250   !  Bottom (X-windows)
             ipic(0:ixdim-1,           0:30   )=200   !  Top
             ipic(ixdim-30:ixdim-1  ,0:iydim-1)=150   !  Right
             ipic(0:30              ,0:iydim-1)=100   !  Left
             do k=0,iydim-100
                ipic(k:k+50,k)=188
             enddo
             ipic(ixdim/4:3*ixdim/4,iydim/4:3*iydim/4)=250
      endif
c
c ##################################################################
c                           End of action 2
c ##################################################################
c
      if(nsteps.eq.3   !  Create a grid pattern
     *  )then
             bad_action=.false.
             ixdim=1024
             iydim=1024
             allocate(ipic (0:ixdim-1,0:iydim-1))
             allocate(rpic (0:ixdim-1,0:iydim-1))
c
c            kxa=ixdim/4
c            kxb=3*kxa
c            kya=iydim/5
c            kyb=4*kya
             kxa=0
             kxb=ixdim-1
             kya=0
             kyb=iydim-1
             write(*,*)'Integer extra width of grid bars?'
             read (*,*)ld
c
             do ix=kxa,kxb,64
                ipic(ix:ix+ld,kya:kyb)=250
             enddo
c
             do iy=kya,kyb,64
                ipic(kxa:kxb,iy:iy+ld)=250
             enddo
      endif
c
c ##################################################################
c                           End of action 3
c ##################################################################
c
      if(nsteps.eq.4   !  Create a white rectangle
     *  )then
             bad_action=.false.
c
             ixdim=640
             iydim=480
             allocate(ipic (0:ixdim-1,0:iydim-1))
             allocate(rpic (0:ixdim-1,0:iydim-1))
c
             ix2=ixdim/2
             iy2=iydim/2
c
             write(*,*)'Pixel width of rectangle (-ve ==> top left)'
             read(*,*)kwr
c
             write(*,*)'Pixel height of rectangle'
             read(*,*)khr
c
             if(kwr.lt.0
     *         )then
                    kxa=0
                    kya=0
                    kwr=-kwr
                else
                    kxa=ix2-kwr/2
                    kya=iy2-khr/2
             endif
c
             kxb=kxa+kwr-1
             kyb=kya+khr-1
c
             write(*,*)kxa,kxb,kya,kyb
             ipic(kxa:kxb,kya:kyb)=250
      endif
c
c ##################################################################
c                           End of action 4
c ##################################################################
c
      if(bad_action
     *  )then
             write(*,*)'0 - complex number demo'
             write(*,*)'1 - read a JPEG file'
             write(*,*)'2 - grid pattern'
             write(*,*)'3 - white rectangle'
             stop
         else
             write(*,*)'Drawing original image'
             rpic=char(ipic)
             bmname='0_start_image'
             write(*,*)bmname
             call array2bmp(ixdim,iydim,rpic)
             write(*,*)'Drawn original image'
      endif
c
c       Find the smallest-power-of-two array side that will hold this data.
c
      sizedim=amax0(ixdim,iydim)
      do n=1,16
         if(2**n.ge.sizedim)exit
      enddo
      sizedim=2**n
      rsize=1./dfloat(sizedim**2)
      write(*,*)'Array edge is',sizedim

      open(30,form='unformatted',status='unknown')
         write(30)sizedim     ! write out the length of the square
      close(30)
c
      allocate(cpsf    (sizedim,sizedim))
      allocate(cpicdata(sizedim,sizedim))   ! Was cw...
      allocate(cx        (sizedim,sizedim),cy(sizedim,sizedim))
      allocate(c2df_image(sizedim,sizedim))
      allocate(c2df_psf  (sizedim,sizedim))
      allocate(w(sizedim))
c
c      Transfer the integer picture data to the 
c      centre of a complex(8), power_of_2, array
c
c     kxa=1                            ! Top left
c     kya=1                            ! Top left
c
      kxa=(sizedim/2)-(ixdim/2)+1      ! Middle
      kya=(sizedim/2)-(iydim/2)+1      ! Middle
c
      kxb=kxa+ixdim-1
      kyb=kya+iydim-1
c
      cpicdata=dcmplx(0.0,0.0)
      iya=0
      do iy=kya,kyb
         ixa=0
         do ix=kxa,kxb
            cpicdata(ix,iy)=dcmplx(dfloat(ipic(ixa,iya)),0.0)
            ixa=ixa+1
         enddo
         iya=iya+1
      enddo
c
c      Normalise the image
c
      rn=1.0/maxval(real(cpicdata))
      cpicdata=rn*cpicdata
c
      write(*,*)'Normalistion of image complete'
c
c
      deallocate(ipic)   !   No longer required
c
c      initialise the FFT constants
c
      call Cffti(w,sizedim)
c
c      Do the forward 2-D FFT without transposing the image
c
      cx=cpicdata
      cy=(0.0,0.0)
      write(*,*)' '
      write(*,*)'Starting forward transform of image'
      call Cfftf(sizedim,sizedim,cx,cy,w,.True.)
      call Cfftf(sizedim,sizedim,cx,cy,w,.false.)
      c2df_image=rsize*cx   ! The normalised forward transform of the image
c
      open(32,form='unformatted',status='unknown')
         write(32)c2df_image  ! Write out the normalised image transform
      close(32)
      write(*,*)'Forward transform of image written to fort.32'
c      
c      Create a Gaussian PSF
c
      rsb=-1./(2.0*(sigma_blur**2))
      mb=0.5+(sigma_blur*sqrt(-2.0*log(fringe)))
c
c      Make matrix_blur odd
c
      matrix_blur=1+(2*mb)
      write(*,*)' '
      write(*,*)'Matrix_blur side length is',matrix_blur
      allocate(kernel_psf(-mb:mb,-mb:mb))
      do iy=0,mb
         ay2=dfloat(iy)**2
         do ix=0,mb
            ax2=dfloat(ix)**2
            a=exp((ax2+ay2)*rsb)
            kernel_psf( ix, iy)=dcmplx(a,0.0)
            kernel_psf( ix,-iy)=dcmplx(a,0.0)
            kernel_psf(-ix, iy)=dcmplx(a,0.0)
            kernel_psf(-ix,-iy)=dcmplx(a,0.0)
         enddo
      enddo
c      
c      Stick this kernel in cpsf
c      
      cpsf=dcmplx(0.0,0.0)
      ka=(sizedim/2)-mb    !  In the middle
c     ka=1                 !  At top left
      kb=ka+(2*mb)
      cpsf(ka:kb,ka:kb)=kernel_psf
c      
c      Create the forward transform of this PSF
c      
      cx=cpsf
      cy=(0.0,0.0)
      write(*,*)' '
      write(*,*)'Starting forward transform of PSF'
      call Cfftf(sizedim,sizedim,cx,cy,w,.True.)
      call Cfftf(sizedim,sizedim,cx,cy,w,.false.)
      c2df_psf=rsize*cx   !  The normalised forward transform of the PSF
c
      open(34,form='unformatted',status='unknown')
         write(34)c2df_psf    ! Write out the normalised PSF transform
      close(34)
      write(*,*)'Forward transform of PSF written to fort.34'
c
      if(sizedim.le.32
     *  )then
             open(30,file='transforms.dat',form='formatted'
     *           ,status='unknown')
c
             do ny=1,sizedim
                do nx=1,sizedim
                   write(30,127)nx,ny,c2df_image(nx,ny),c2df_psf(nx,ny)
     *                                       ,cx(nx,ny)
                enddo
             enddo
c
             close(30)
      endif
c
c
c
      write(*,*)' '
      if(nsteps.ne.1
     *  )then
c
c      Take the inverse transform of the product of image and PSF DFTs
c
c      Multiply image and PSF
c
c   The convolution theorem states that: 
c
c                 The Fourier transform of a convolution 
c                 is the pointwise product 
c                 of Fourier transforms
c
c      Multiplying the real and imaginary parts of (a) 
c             with the real and imaginary parts of (b), 
c        generates the real and imaginary parts of (c).
c
c              (http://www.dspguide.com/ch24/6.htm)
c
             write(*,*)'Creating product of forward transforms'
             cx=c2df_image*c2df_psf
             write(*,*)'Writing forward transform product to fort.36'
         else
             write(*,*)'Divide image transform by PSF'
             cx=c2df_image/c2df_psf
             write(*,*)'Writng divided transform product to fort.36'
      endif
c
      open(36,form='unformatted',status='unknown')
      write(36)cx          ! Write out the transform
      close(36)
c
      stop
 97   continue
      write(*,125)
      stop
 98   continue
      write(0,101)
      stop
c
c
101   format('ERROR in read')
110   format('Picture type is >',a4,'<.  Dimensions are '
     *       ,i0,'x',i0,' - ',i10,' pixels')
125   format('Must supply:'
     *,/'           Action ('
     *,/'                   0 - Complex number arithmetic demo'
     *,/'                   1 - Read from a JPEG file'
     *,/'                   2 - Create a diagonal pattern'
     *,/'                   3 - Create a grid pattern'
     *,/'                   4 - Create a white rectangle'
     *,/'                  )'
     *,/'           Fringe level'
     *,/'           Sigma')
127   format('cx(',i0.0,',',i0.0,')=',e17.9,' +',e17.9,'i '
     *                             ,2(e17.9,' +',e17.9,'i '))
c
      end
