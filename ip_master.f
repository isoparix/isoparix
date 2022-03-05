      program ip_master
c
      use bmp_comms
      use ip_comms
c
c      Gets jpg pixels, does stuff to them, and writes bitmap
c
      real(8)brighten,ta,tb
c
      logical distributed
c
      character (4) pictype
      character (5) charix,chariy
      character (8) convolution_type
      character (20) char_thresh
      character(1),allocatable,dimension(:,:) :: histpic
      integer  (2),allocatable,dimension(:,:) :: cr2pic
      real(8)     ,allocatable,dimension(:,:) :: light
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
      call getarg(1,char_thresh)
      read(char_thresh,*,end=97)nsteps
c
      call getarg(2,char_thresh)
      read(char_thresh,*,end=97)nthresh
c
      call getarg(3,char_thresh)
      read(char_thresh,*,end=97)fringe
c
c     Fringe: Level of Gaussian curve at centre edges of square array
c
      call getarg(4,char_thresh)
      read(char_thresh,*,end=97)sigma_blur
c
      call getarg(5,char_thresh)
      read(char_thresh,*,end=97)lo_contrast
c
      call getarg(6,char_thresh)
      read(char_thresh,*,end=97)hi_contrast
c
      write(*,122)nthresh,fringe,sigma_blur,lo_contrast,hi_contrast
      write(8,122)nthresh,fringe,sigma_blur,lo_contrast,hi_contrast
c
      nxmax=0
      nxmin=1000000
      nymax=0
      nymin=1000000
      histarray=255
c
      call picdims(pictype,ixdim,iydim)   ! How big is it?  What type?
c
      npixels=ixdim*iydim
      write(*,110)pictype,ixdim,iydim,npixels
c
      ncell=float(npixels)/256.
      allocate(picdata   (0:ixdim-1,0:iydim-1))
      allocate(ipic      (-100:ixdim+99,-100:iydim+99))
      allocate(brightness(-100:ixdim+99,-100:iydim+99))
      allocate(light     (-100:ixdim+99,-100:iydim+99))
c
      picdata=char(0)
      ix=0
      iy=0
      nhisto=0
c
      open(1,file='pic.dat',form='unformatted'
     *       ,access='stream',status='old')
c
      allocate(cr2pic(0:ixdim-1,0:iydim-1))
      read(1,iostat=irc,err=98)picdata
c
      ipic=0
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ipic(ix,iy)=ichar(picdata(ix,iy))
         enddo
      enddo
      call ip_histo
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ipic(ix,iy)=ipic(ix,iy)-nthresh  !   Haze removal (or addition...)
            if(ipic(ix,iy).gt.255
     *        )then
                   ipic(ix,iy)=255
            endif
            if(ipic(ix,iy).lt.0
     *        )then
                   ipic(ix,iy)=0
            endif
         enddo
      enddo
c
c      For further processing...
c
      open(20,file='picture.dat',status='unknown'
     *       ,form='unformatted')
      write(20)ixdim,iydim
      write(20)dfloat(ipic)
      close(20)
c
      if(nsteps.eq.0
     *  )then   !   This is just an inspection view
             stop
      endif
c
      if(nsteps.eq.1)convolution_type='gaussian'
      if(nsteps.eq.2)convolution_type='circular'
c
      picdata=char(ipic)
      call ip_histo
c
c      Now have all levels set between 1 and 255-nthresh.
c      Expand to occupy levels between lo_contrast and hi_contrast
c
      b=float(hi_contrast-lo_contrast)/float(maxnzp)  !  Expansion
      write(*,*)b
      do iy=0,iydim-1
         do ix=0,ixdim-1
            if(ipic(ix,iy).gt.0
     *        )then
                  ipic(ix,iy)=int(.5+(b*float(ipic(ix,iy))))+lo_contrast
                  if(ipic(ix,iy).gt.255
     *              )then
                         ipic(ix,iy)=255
                  endif
            endif
         enddo
      enddo
c
      picdata=char(ipic)
      call ip_histo
      bmname='ip_broadened'
      call array2bmp(ixdim,iydim,picdata)
c
      call tim(ta)
      call ip_sharpen(convolution_type)
      call tim(tb)
      tb=tb-ta
c
      write(*,124)tb
c
         call ip_histo
         write(bmname,300)20
         call array2bmp(ixdim,iydim,picdata)
c
 60   continue
      bmname='ip_histogram'
      ndepth=nhisto*265
      allocate(histpic(800,ndepth))
      do iy=1,ndepth
         do ix=1,800
            histpic(ix,iy)=char(histarray(ix,iy))
         enddo
      enddo
      call array2bmp(800,ndepth,histpic)
c
      stop
 97   continue
      write(*,125)
      stop
 98   continue
      stop
c
100   format(24a1)
101   format('ERROR in read')
102   format('X,',i6,',',i6)
103   format('Y,',i6,',',i6)
1041  format('Top  : ',6f8.1)
1042  format('Bot  : ',6f8.1)
1043  format('Left : ',6f8.1)
1044  format('Right: ',6f8.1)
105   format('Original frame:',4i8,f6.3)
106   format('     New frame:',4i8,f6.3,i8)
107   format(/'Numbers: Left=',i8,', Right=',i8
     *       ,', Bottom=',i8,', Top=',i8,', Landscape=',L1)
108   format('convert -crop ',i0,'x',i0,'+',i0,'+',i0
     *      ,' %1.jpg %1_crop.jpg')
109   format(200a1)
110   format('Picture type is >',a4,'<.  Dimensions are '
     *       ,i0,'x',i0,' - ',i10,' pixels')
111   format(i8,' black pixels located - current line is',i8)
112   format(/i8,' black pixels located - all pixels read')
113   format(a$)
114   format('X=',i5,i4,i5,2f6.1,l2,' ',40z3)
115   format('ERROR in stream identity text: ',80a1)
116   format('Y=',i5,i4,i5,2f6.1,l2,' ',40z3)
117   format(25i5)
118   format(3i6)
120   format(16(16i8,/))
121   format(/f8.3,2x,a)
122   format('Threshold:',i4,' Fringe:',f8.3,' Sigma:',f8.2
     *      ,', Lo-contrast:',i4,', Hi_contrast:',i4)
123   format('Start of sharpening at',2a12)
124   format('Sharpening took',f8.2,' seconds')
125   format('Must suppy: Threshold, Base-matrix, Sigma, Lo-contrast'
     *      ,', Hi_contrast')
200   format('IP_MASTER OK TO HERE...')
300   format('IP_sharpen_',i2.2)
301   format('IP_sharpen_contrast',i2.2)
c
      end
