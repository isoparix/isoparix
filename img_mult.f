      program img_mult
c
      use bmp_comms
      use ip_comms
c
c      Reads images, find the centre of gravity of each, multiplies them
c      together, and normalises the result
c
      real(8)brighten,ta,tb
c
      logical distributed,faint
c
      character (4) pictype
      character (5) charix,chariy
      character (20) char_thresh
      character (20) image_collection
      character (20) image_name
      character(1),allocatable,dimension(:,:) :: histpic
      integer  (2),allocatable,dimension(:,:) :: cr2pic
      real(8)     ,allocatable,dimension(:,:) :: light
      real(8)     ,allocatable,dimension(:)   :: delta
c
      character(1)rgb(0:1023)
      character(1024)rgbq,stream_cmd
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
      call getarg(1,image_collection)
c
      nxmax=0
      nxmin=1000000
      nymax=0
      nymin=1000000
      histarray=255
c
      open(3,file=image_collection,form='formatted',status='unknown')
      npics=0
   1  continue
      read(3,*,end=2,err=98)image_name
      npics=npics+1
      stream_cmd='stream -identify -map i -storage-type char '
     *           //trim(image_name)//' pic.dat > fort.7'
      call system(stream_cmd)
      call picdims(pictype,ixdim,iydim)   ! How big is it?  What type?
c
      npixels=ixdim*iydim
      write(*,110)pictype,ixdim,iydim,npixels
      ncgx=1500  ! These are high
      ncgy=1500  ! variation areas
c
      ncell=float(npixels)/256.
      if(npics.eq.1
     *  )then
             allocate(picdata   (0:ixdim-1,0:iydim-1))
             allocate(ipic      (-300:ixdim+299,-300:iydim+299))
             allocate(jpic      (-300:ixdim+299,-300:iydim+299))
             allocate(brightness(-300:ixdim+299,-300:iydim+299))
             allocate(light     (-300:ixdim+299,-300:iydim+299))
      endif
      ipic=0
      jpic=0
      light=1.0
c
      picdata=char(0)
      ix=0
      iy=0
      nhisto=0
c
      open(1,file='pic.dat',form='unformatted'
     *       ,access='stream',status='old')
      read(1,iostat=irc,err=98)picdata
      close(1)
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ipic(ix,iy)=ichar(picdata(ix,iy))
         enddo
      enddo
      if(npics.eq.1
     *  )then
             jpic=ipic
      endif
c
      nrange=50
      ixl=ncgx-200
      ixh=ncgx+200
      iyl=ncgy-200
      iyh=ncgy+200
c
      nsize=(ixh-ixl+1)*(iyh-iyl+1)
      if(.not.allocated(delta))allocate(delta(nsize))
c
c      Cut the picture into small squares and find the mean and variance
c      of each one
c
      varlow=huge(varlow)
      varhi =0.
      do ny=nrange,iydim-1-nrange,2*nrange ! Move the centre of the box around
         do nx=nrange,ixdim-1-nrange,2*nrange !
            ndiff=0
            asum=0
            faint=.false.
            do iy=-nrange,nrange    ! Sum within a box 
               do ix=-nrange,nrange ! (2*nrange+1)**2
                  ndiff=ndiff+1
                  delta(ndiff)=ipic(ix+nx,iy+ny)!-jpic(ix+ncgx,iy+ncgy)
c                 if(ipic(ix+nx,iy+ny).lt.20
c    *              )then
c                        faint=.true.
c                        exit
c                 endif
                  asum=asum+delta(ndiff)
               enddo
            enddo
            if(.not.faint
     *        )then
c
c      Calculate mean and variance of the differences
c
            amean=asum/real(ndiff)
            var=0.
            do n=1,ndiff
               var=var+(delta(n)-amean)**2
            enddo
            var=var/real(ndiff)
            sd=sqrt(var)
            sdn=sd/amean
c
            write(8,117)npics,amean,var,sd,sdn,nx,ny
c
            if(var.gt.varhi
     *        )then
                   varhi=var
                   amhi=amean
                   sdhi=sd
                   sdnhi=sdn
                   nxhi=nx
                   nyhi=ny
            endif
c
            if(var.lt.varlow
     *        )then
                   varlow=var
                   sdlow=sd
                   sdnlow=sdn
                   amlow=amean
                   nxlow=nx
                   nylow=ny
            endif
            endif  ! Not faint
c
         enddo
      enddo
      idx=ncgx-lownx
      idy=ncgy-lowny
      write(*,117)npics,amlow,varlow,sdlow,sdnlow,nxlow,nylow
     *                 ,amhi ,varhi ,sdhi ,sdnhi ,nxhi ,nyhi,ndiff
c
c     call ip_histo
c
c     Align centres of gravity with centre of array 'light'
c
c     ncgx=icogx-ncgx
c     ncgy=icogy-ncgy
c     write(*,*)icogx,ncgx,ncgx,icogy,ncgy,ncgy
c     do iy=0,iydim-1
c        iyn=iy-ncgy
c        do ix=0,ixdim-1
c           light(ix-ncgx,iyn)=real(ipic(ix,iy))*light(ix-ncgx,iyn) ! Multiply
c        enddo
c     enddo
      go to 1
   2  continue
c
c     ipic=light**(1.0/real(npics))
c
c     picdata=char(ipic)
c     call ip_histo
c     bmname='convolved'
c     call array2bmp(ixdim,iydim,picdata)
c
      stop
  98  stop
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
117   format(i8,2(4f12.2,2i6),6i6)
118   format(3i6)
120   format(16(16i8,/))
121   format(/f8.3,2x,a)
122   format('Threshold:',i4,' Base-matrix:',f8.3,' Sigma:',f8.2
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
