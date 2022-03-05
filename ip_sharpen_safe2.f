      subroutine ip_sharpen(convolution_type)
c
      use ip_comms
      use bmp_comms
c
c      Assign 2D-gaussian values to con_matrix 
c
      integer(4),allocatable,dimension(:,:) :: cmlog
c
      character(1),allocatable,dimension(:,:) :: cm_pic
      character(8)convolution_type
      character(30)blur_name
c
      logical non_compute(0:255)
c
      real(8),allocatable,dimension(:,:,:) :: cm_num
      real(8),allocatable,dimension(:,:)   :: con_matrix,blur_array
      real(8),allocatable,dimension(:)     :: ax2
c
      if(convolution_type.eq.'gaussian'
     *  )then
             matrix_blur=sigma_blur*sqrt(-2.0*log(fringe))
             go to 2
      endif
c
      if(convolution_type.eq.'circular'
     *  )then
             matrix_blur=sigma_blur
             go to 2
      endif
c
      write(0,115)convolution_type
      stop
c
  2   continue
c
      open(38,file='ipsharpen.log',form='formatted',status='unknown')
      write(blur_name,113)matrix_blur
      open(8,file=trim(blur_name),status='unknown')
      write(*,109)convolution_type,matrix_blur,sigma_blur
      write(38,109)convolution_type,matrix_blur,sigma_blur
c
      allocate(ax2(0:matrix_blur))
      allocate(con_matrix(-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur)
     *        )
      allocate(cmlog     (-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur))
      allocate(cm_num    (           0:255
     *                   ,-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur)
     *        )
      allocate(cm_pic    (-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur))
      allocate(conarray(0:ixdim-1,0:iydim-1))
      allocate(blur_array(-matrix_blur:ixdim+matrix_blur-1
     *                   ,-matrix_blur:iydim+matrix_blur-1)
     *        )
c
      r255=1.0/255.0
c
      do ix=0,matrix_blur
         ax2(ix)=float(ix*ix)
      enddo
c
      rsb=-1./sigma_blur
      write(*,111)matrix_blur,sigma_blur,fringe
      con_matrix=0.0
      nfalse=0
      cmlog=0
      mb=0
      if(convolution_type.eq.'gaussian'
     *  )then
             do iy=0,matrix_blur
                ay2=float(iy*iy)
                do ix=0,matrix_blur
                   con_matrix(ix,iy)=
     *                             exp(-0.5*(sqrt(ax2(ix)+ay2)*rsb)**2)
                enddo
             enddo
             go to 1
      endif
c
      if(convolution_type.eq.'circular'
     *  )then
             do iy=0,matrix_blur
                ay2=float(iy*iy)
                do ix=0,matrix_blur
                   ir=0.5+sqrt(ax2(ix)+ay2)
                   if(ir.le.matrix_blur)con_matrix(ix,iy)=0.5
                enddo
             enddo
             go to 1
      endif
c
      write(0,115)convolution_type
      stop
c
  1   continue
c       Create an image of the convolution matrix
c
      cm_pic(0:matrix_blur,0:matrix_blur)
     *                 =char(int(.5+(255.0*con_matrix)))
      cm_num(255,0:matrix_blur,0:matrix_blur)=con_matrix
      do k=0,matrix_blur
         do j=0,matrix_blur
c
            con_matrix( j,-k)=con_matrix(j,k)
            con_matrix(-j, k)=con_matrix(j,k)
            con_matrix(-j,-k)=con_matrix(j,k)
c
         enddo
      enddo
c
      write(38,*)'CON_MATRIX'
      do iy=-matrix_blur,matrix_blur
         write(38,103)con_matrix(:,iy)
      enddo
c
      resa=sum(con_matrix)
      rsum=1.0/resa
      write( *,1031)resa
      write(38,1031)resa
c
c     open(20,file=convolution_type//'.dat',form='unformatted'
c    *       ,status='unknown')
c        ndim=1+(2*matrix_blur)
c        write(20)ndim,ndim
c        write(20)con_matrix
c     close(20)
c
      bmname='convolution'
      cm_pic=char(int(255.0*con_matrix))
      call array2bmp(1+(2*matrix_blur),1+(2*matrix_blur),cm_pic)
c 
      do idensity=0,255
         cm_num(idensity,:,:)=con_matrix*float(idensity)
      enddo
c
      blur_array=0.0  !  All values taken from density matrix
c
      write(*,104),ixdim,iydim,matrix_blur
      non_compute=.true.
      nzero=0
      do iy=0,iydim-1
c        write(*,117)iy,nzero
         do ix=0,ixdim-1
            idensity=ipic(ix,iy)
            if(idensity.gt.0
     *        )then
                   blur_array(ix-matrix_blur:ix+matrix_blur
     *                       ,iy-matrix_blur:iy+matrix_blur) =
     *             blur_array(ix-matrix_blur:ix+matrix_blur
     *                       ,iy-matrix_blur:iy+matrix_blur)
     *            +    cm_num(idensity,:,:)
               else
                   nzero=nzero+1    
            endif
         enddo
      enddo
      write(*,*)nzero
      conarray=blur_array(0:ixdim-1,0:iydim-1)
      write(*,105 )maxval(int(conarray)),minval(int(conarray))
      conarray=conarray*rsum
      write(*,105 )maxval(int(conarray)),minval(int(conarray))
      write(*,1052)maxval(ipic),minval(ipic)
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ip=(2*ipic(ix,iy))-int(conarray(ix,iy))
c           ip=(3*ipic(ix,iy)/2) -int(conarray(ix,iy))
            if(ip.gt.255
     *        )then
                   ip=255
c                  write(38,*)'Too high at',ix,iy,ip
            endif
            if(ip.lt.0
     *        )then
c                  write(38,*)' Too low at',ix,iy,ip
                   ip=0
            endif
            picdata(ix,iy)=char(ip)
         enddo
      enddo
      write(*,1051)maxval(ichar(picdata)),minval(ichar(picdata))
      close(38)
c
      return
c
100   format('max/min(conarray),rtop,max/min(picdata):',3f10.4,2i10)
101   format(6i8,4f10.3)
102   format(//)
103   format(100f10.6)
1031  format('IP_SHARPEN: con_matrix sum=',f20.5)
104   format('IP_SHARPEN: Blurring array of',i6,' by',i6,'. Matrix:',i6)
105   format('IP_SHARPEN: conarray max/min',2i8)
1051  format('IP_SHARPEN:  picdata max/min',2i8)
1052  format('IP_SHARPEN:     ipic max/min',2i8)
1053  format('IP_SHARPEN: blur_array max/min',2i8)
107   format('IP_SHARPEN: conarray > brightness at',2i6,2e20.12)
108   format('IP_SHARPEN: ',3i6,f10.5)
109   format('IP_SHARPEN: Blur matrix (',a8') edge is 0:',i0.0
     *      ,', Sigma is:',f10.5)
110   format('IP_SHARPEN: Row',i3,': Last usable value=',f6.3,' at',i3)
111   format('IP_SHARPEN: matrix_blur',i6,', sigma_blur',f10.5
     *      ,', fringe',f10.5)
112   format('sigma',i4.4,'_fringe_',i4.4)
113   format('conv_matrix_',i0.0,'.dat')
114   format(i8,50i3)
115   format('Convolution type of >',a,'< not recognised')
116   format('Computing blur matrix for density',i4
     *      ,' at IX=',i5,', IY=',i5)
117   format('Starting row',i5,'.  Number of zero points=',i8)
c
      end
