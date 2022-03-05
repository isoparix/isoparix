      program ip_slopes
c
      use ip_comms
      use bmp_comms
c
c      Derives slopes between adjacent pixels
c
      character(1),allocatable,dimension(:,:) :: cm_pic
      integer nslope(-255:255) 
c
      open(20,file='picture.dat',status='old',form='unformatted')
      read(20)ixdim,iydim
      write(*,*)ixdim,iydim
c
      allocate(ipic(0:ixdim-1,0:iydim-1))
c
      read(20)ipic
      close(20)
      
c
      ixh=ixdim/2
      min_delta= 1000000
      max_delta=-1000000
      nslope=0
      do iy=0,iydim-2
         do ix=0,ixdim-2
c
            ndelta=abs(ipic(ix,iy)-ipic(ix+1,iy))
            if(ndelta.gt.max_delta)max_delta=ndelta
            if(ndelta.lt.min_delta)min_delta=ndelta
            nslope(ndelta)=nslope(ndelta)+1
c
            ndelta=ipic(ix,iy)-ipic(ix,iy+1)
            if(ndelta.gt.max_delta)max_delta=ndelta
            if(ndelta.lt.min_delta)min_delta=ndelta
            nslope(ndelta)=nslope(ndelta)+1
c
         enddo
      enddo
c
      write(8,101)min_delta,max_delta
      do n=-255,255
         write(8,101)n,nslope(n)
      enddo
      stop
c
100   format('max/min(conarray),rtop,max/min(ipic):',3f10.4,2i10)
101   format(i10,',',i10)
102   format(//)
103   format(100f10.6)
114   format(i8,50i3)
c
      end
