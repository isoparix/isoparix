      subroutine ip_sharpen
c
      use ip_comms
      use bmp_comms
c
c      Assign 2D-gaussian values to con_matrix 
c
      integer(4),allocatable,dimension(:)   :: cml
      integer(4),allocatable,dimension(:,:) :: jpic
c
      character(1),allocatable,dimension(:,:) :: cm_pic
c
      real(8),allocatable,dimension(:,:) :: con_matrix
      real(8),allocatable,dimension(:)   :: ax2
c
      fringe=.1
      matrix_blur=sigma_blur*sqrt(-2.0*log(fringe))
      write(*,109)matrix_blur,sigma_blur
c
      allocate(ax2(0:matrix_blur))
      allocate(con_matrix(0:matrix_blur,0:matrix_blur))
      allocate(cml       (0:matrix_blur))
      allocate(cm_pic    (-matrix_blur:matrix_blur
     *                   ,-matrix_blur:matrix_blur))
      allocate(conarray(0:ixdim-1,0:iydim-1))
      allocate(jpic    (-100:iydim+99,-100:ixdim+99))   !  For transpose of ipic
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
      cml=0
      mb=0
      do iy=0,matrix_blur
         ay2=float(iy*iy)
         do ix=0,matrix_blur
            con_matrix(ix,iy)=exp(-0.5*(sqrt(ax2(ix)+ay2)*rsb)**2)
         enddo
      enddo
c
      do iy=0,matrix_blur
         write(8,103)con_matrix(:,iy)
      enddo
      call flush(8)
c
c       Create an image of the convolution gaussian
c
      cm_pic(0:matrix_blur,0:matrix_blur)
     *                 =char(int(.5+(255.0*con_matrix)))
      do k=0,matrix_blur
         do j=0,matrix_blur
            cm_pic( j,-k)=cm_pic(j,k)
            cm_pic(-j, k)=cm_pic(j,k)
            cm_pic(-j,-k)=cm_pic(j,k)
         enddo
      enddo
      bmname='gaussian'
      call array2bmp(1+(2*matrix_blur),1+(2*matrix_blur),cm_pic)
c
      ntrue=((1+(2*matrix_blur))**2)-(4*nfalse)
      resa= 1.0+(4.0*sum(con_matrix(1:matrix_blur,:)))
      rsum=1./resa
      write(*,1031)resa,ntrue,(1+(2*matrix_blur))**2
      write(8,1031)resa,ntrue,(1+(2*matrix_blur))**2
      call flush(8)
c
c      Create transpose of ipic
c
      write(*,113)
      jpic=transpose(ipic)
c     do iy=0,iydim-1
c        do ix=0,ixdim-1
c           jpic(iy,ix)=ipic(ix,iy)
c        enddo
c     enddo
      write(*,114)
c
      write(*,104),ixdim,iydim,matrix_blur
      do iy=0,iydim-1
         do ix=0,ixdim-1
            res=ipic(ix,iy)
            do k=1,matrix_blur
c              res=res+(4.0*con_matrix(k,0))
               t=ipic(ix+k,iy)
     *          +ipic(ix-k,iy)
            enddo
            do k=1,matrix_blur
     *         t=t+jpic(iy+k,ix)
     *            +jpic(iy-k,ix)
            enddo
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
                  t=jpic(iy+l,ix+k)
     *             +jpic(iy-l,ix+k)
     *             +jpic(iy+l,ix-k)
     *             +jpic(iy-l,ix-k)
     *             +ipic(ix+l,iy+k)
     *             +ipic(ix-l,iy+k)
     *             +ipic(ix+l,iy-k)
     *             +ipic(ix-l,iy-k)
                  res=res+(con_matrix(l,k)*t)
               enddo
            enddo
            conarray(ix,iy)=res*rsum
         enddo
      enddo
c
      do iy=0,iydim-1
         do ix=0,ixdim-1
            ip=(2*ipic(ix,iy))-int(conarray(ix,iy))
            if(ip.gt.255)ip=255
            if(ip.lt.0)  ip=0
            picdata(ix,iy)=char(ip)
         enddo
      enddo
      write(*,105)maxval(ichar(picdata)),minval(ichar(picdata))
c
c      For further processing...
c
      open(20,file='blurred.dat',status='unknown',form='unformatted')
      write(20)ixdim,iydim
      write(20)ipic
      write(20)conarray
      close(20)
c
      return
c
100   format('max/min(conarray),rtop,max/min(picdata):',3f10.4,2i10)
101   format(6i8,4f10.3)
102   format(/)
103   format(100f10.6)
1031  format('IP_SHARPEN: con_matrix sum=',f10.5,'. NTRUE=',i5,' of',i5)
104   format('IP_SHARPEN: Blurring array of',i6,' by',i6,'. Matrix:',i6)
105   format('IP_SHARPEN: Blurring complete',2i12)
107   format('IP_SHARPEN: conarray > brightness at',2i6,2e20.12)
108   format('IP_SHARPEN: ',3i6,f10.5)
109   format('IP_SHARPEN: ',i6,2f10.5)
110   format('IP_SHARPEN: Row',i3,': Last usable value=',f6.3,' at',i3)
111   format('IP_SHARPEN: matrix_blur',i6,', sigma_blur',f10.5
     *      ,', fringe',f10.5)
112   format('sigma',i4.4,'_fringe_',i4.4)
113   format('Transpose starts...')
114   format('Transpose ends...')
c
      end
