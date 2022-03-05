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
     *                             ,cx,cy,kernel_psf,c2df_psf
      complex(8)  ,allocatable,dimension(:)   :: w
      complex(8)cxa,cxb,cxc,cxd,cxe
c
      bad_action=.true.
c
      call greymap
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
      nxmax=0
      nxmin=1000000
      nymax=0
      nymin=1000000
c
      if(nsteps.eq.1   ! Reading from a JPEG file
     *  )then
             bad_action=.false.
             call picdims(pictype,ixdim,iydim)   ! How big is it?  What type?
             npixels=ixdim*iydim
             write(*,110)pictype,ixdim,iydim,npixels
c
             allocate(picdata   (0:ixdim-1,0:iydim-1))
c
             picdata=char(0)
c
             open(1,file='pic.dat',form='unformatted'
     *              ,access='stream',status='old')
c
c            allocate(cr2pic(0:ixdim-1,0:iydim-1))
             read(1,iostat=irc,err=98)picdata
c
      endif
c
      if(bad_action
     *  )then
             stop
         else
             bmname='0_start_image'
             call array2bmp(ixdim,iydim,picdata)
      endif
c
c       Find the smallest power of two array side that will hold this data.
c
      sizedim=amax0(ixdim,iydim)
      do n=1,16
         if(2**n.ge.sizedim)exit
      enddo
      sizedim=2**n
      rsize=1./dfloat(sizedim**2)
      write(*,*)'Array edge is',sizedim
c
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
            cpicdata(ix,iy)=dcmplx(dfloat(ichar(picdata(ixa,iya))),0.0)
            ixa=ixa+1
         enddo
         iya=iya+1
      enddo
c
c      Normalise the image
c
      rn=1.0/maxval(real(cpicdata))
      cpicdata=rn*cpicdata
      call quad_transpose(cpicdata,sizedim)
c
      write(*,*)'Normalisation and tranposition of image complete'
c
c      initialise the FFT constants
c
      call Cffti(w,sizedim)
c
c      Do the forwards 2-D FFT
c
      cx=cpicdata
      cy=(0.0,0.0)
      write(*,*)' '
      write(*,*)'Starting forward transform of image'
      call Cfftf(sizedim,sizedim,cx,cy,w,.True.)
      call Cfftf(sizedim,sizedim,cx,cy,w,.false.)
      c2df_image=rsize*cx   ! The normalised forward transform of the image
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
c      Stick this kernel in cx
c      
      cx=dcmplx(0.0,0.0)
      ka=(sizedim/2)-mb    !  In the middle
c     ka=1                 !  At top left
      kb=ka+(2*mb)
      cx(ka:kb,ka:kb)=kernel_psf
c      
c      Create the forward transform of this PSF
c      
      cy=(0.0,0.0)
      write(*,*)' '
      write(*,*)'Starting forward transform of PSF'
      call Cfftf(sizedim,sizedim,cx,cy,w,.True.)
      call Cfftf(sizedim,sizedim,cx,cy,w,.false.)
      c2df_psf=rsize*cx   !  The normalised forward transform of the PSF
c
c   The convolution theorem states that: 
c
c     The Fourier transform of a convolution 
c                 is the pointwise product of Fourier transforms
c
c      Multiplying the real and imaginary parts of (a) 
c             with the real and imaginary parts of (b), 
c        generates the real and imaginary parts of (c).
c
c              (http://www.dspguide.com/ch24/6.htm)
c
      write(*,*)'Divide image forwd transform by PSF forwd transform'
      cx=c2df_image/c2df_psf
      write(*,*)'Creating backward transform c2df_image/c2df_PSF'
c
      CALL Cfftb(sizedim,sizedim,cx,cy,w,.True.)
      CALL Cfftb(sizedim,sizedim,cx,cy,w,.false.)
      write(bmname,100)
      call ip_fft_display(sizedim,sizedim,cx)
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
100   format('sharpened')
101   format('ERROR in read')
110   format('Picture type is >',a4,'<.  Dimensions are '
     *       ,i0,'x',i0,' - ',i10,' pixels')
125   format('Must suppy: Threshold, Base-matrix, Sigma, Lo-contrast'
     *      ,', Hi_contrast')
127   format('cx(',i0.0,',',i0.0,')=',e17.9,' +',e17.9,'i '
     *                             ,2(e17.9,' +',e17.9,'i '))
c
      end
