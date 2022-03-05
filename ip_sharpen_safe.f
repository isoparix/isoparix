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
      logical conv_record
c
      real(8),allocatable,dimension(:,:) :: con_matrix,cm_num
      real(8),allocatable,dimension(:)   :: ax2
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

      write(blur_name,113)matrix_blur
      open(8,file=trim(blur_name),status='unknown')
      write(*,109)convolution_type,matrix_blur,sigma_blur
      write(8,109)convolution_type,matrix_blur,sigma_blur
c
      allocate(ax2(0:matrix_blur))
      allocate(con_matrix(0:matrix_blur,0:matrix_blur))
      allocate(cmlog     (-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur))
      allocate(cm_num    (-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur))
      allocate(cm_pic    (-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur))
      allocate(conarray(0:ixdim-1,0:iydim-1))
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
      do iy=0,matrix_blur
         write(8,103)con_matrix(:,iy)
      enddo
c
c       Create an image of the convolution matrix
c
      cm_pic(0:matrix_blur,0:matrix_blur)
     *                 =char(int(.5+(255.0*con_matrix)))
      cm_num(0:matrix_blur,0:matrix_blur)=con_matrix
      do k=0,matrix_blur
         do j=0,matrix_blur
c
            cm_pic( j,-k)=cm_pic(j,k)
            cm_pic(-j, k)=cm_pic(j,k)
            cm_pic(-j,-k)=cm_pic(j,k)
c
            cm_num( j,-k)=cm_num(j,k)
            cm_num(-j, k)=cm_num(j,k)
            cm_num(-j,-k)=cm_num(j,k)
c
         enddo
      enddo
      open(20,file=convolution_type//'.dat',form='unformatted'
     *       ,status='unknown')
         ndim=1+(2*matrix_blur)
         write(20)ndim,ndim
         write(20)cm_num
      close(20)
c
      bmname='convolution'
      call array2bmp(1+(2*matrix_blur),1+(2*matrix_blur),cm_pic)
c
      ntrue=((1+(2*matrix_blur))**2)-(4*nfalse)
      resa= 1.0+(4.0*sum(con_matrix(1:matrix_blur,:)))
      rsum=1./resa
      write(*,1031)resa,ntrue,(1+(2*matrix_blur))**2
      
      write(8,1031)resa,ntrue,(1+(2*matrix_blur))**2
c
c     conarray=conv2(ipic,con_matrix)
c
      write(*,104),ixdim,iydim,matrix_blur
      ixh=ixdim/2
      iyh=iydim/2
      do iy=0,iydim-1
         do ix=0,ixdim-1
            if(ix.eq.ikh.and.iy.eq.iyh
     *        )then
                   conv_record=.true.
               else
                   conv_record=.false.
            endif
            res=ipic(ix,iy)
            do k=1,matrix_blur
c              res=res+(4.0*con_matrix(k,0))
               t=ipic(ix+k,iy  )
     *          +ipic(ix-k,iy  )
     *          +ipic(ix  ,iy+k)
     *          +ipic(ix  ,iy-k)
               res=res+(con_matrix(k,0)*t)
c
c              res=res+(4.0*con_matrix(k,k))
               t=ipic(ix+k,iy+k)
     *          +ipic(ix+k,iy-k)
     *          +ipic(ix-k,iy+k)
     *          +ipic(ix-k,iy-k)
               res=res+(con_matrix(k,k)*t)
c
               do l=1+k,matrix_blur
c                 res=res+(8*con_matrix(l,k))
                  t=ipic(ix+k,iy+l)
     *             +ipic(ix+k,iy-l)
     *             +ipic(ix-k,iy+l)
     *             +ipic(ix-k,iy-l)
     *             +ipic(ix+l,iy+k)
     *             +ipic(ix+l,iy-k)
     *             +ipic(ix-l,iy+k)
     *             +ipic(ix-l,iy-k)
                  res=res+(con_matrix(l,k)*t)
c
               enddo
            enddo
            conarray(ix,iy)=res*rsum
         enddo
      enddo
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ip=(2*ipic(ix,iy))-int(conarray(ix,iy))
c           ip=(3*ipic(ix,iy)/2) -int(conarray(ix,iy))
            if(ip.gt.255)ip=255
            if(ip.lt.0)  ip=0
            picdata(ix,iy)=char(ip)
         enddo
      enddo
      write(*,105)maxval(ichar(picdata)),minval(ichar(picdata))
c
      return
c
100   format('max/min(conarray),rtop,max/min(picdata):',3f10.4,2i10)
101   format(6i8,4f10.3)
102   format(//)
103   format(100f10.6)
1031  format('IP_SHARPEN: con_matrix sum=',f10.5,'. NTRUE=',i5,' of',i5)
104   format('IP_SHARPEN: Blurring array of',i6,' by',i6,'. Matrix:',i6)
105   format('IP_SHARPEN: Blurring complete',2i12)
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
c
      end
