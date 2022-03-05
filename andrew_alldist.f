      program andrew_alldist
c
      use andrew_comms
c
      implicit real(8) (a-h,o-z)
c
      character(10) units
c
c      Reads in gross travel times and delivers finer times 
c      on basis of lon/lat data 
c
      call andrew_get_outcode_ll
c
c     radius=6378.1D+00 ! Kilometres
c     units ='kilometres'
      radius=3963.2D+00 ! Miles
      units ='miles     '
c
      degrad=3.14159265358979D+00/180.0D+00
      ncount=0
      do i=1,noutrecs-1
         alat=degrad*outlat(i)
         alon=degrad*outlon(i)
         dsalat=dsin(alat)
         dcalat=dcos(alat)
c        write(*,*)i
         do j=i+1,noutrecs
            blat=degrad*outlat(j)
            blon=degrad*outlon(j)
c
c      Returns distance on surface of sphere radius r between 
c      lonlatA and lonlatB now in radians.  
c
            dist=radius*(dacos(
     *                      (dsalat*dsin(blat))+
     *                      (dcalat*dcos(blat)*dcos(alon-blon))
     *                        )
     *                  )
c
            ncount=ncount+1
            write(8,102)outcode(i),outlon(i),outlat(i),
     *                  outcode(j),outlon(j),outlat(j),dist
         enddo
      enddo
c
      stop
c
100   format(a10,2i6,2f20.14)
102   format(2(a10,2f12.7),': Spherical surface distance=',f8.3)
      end
