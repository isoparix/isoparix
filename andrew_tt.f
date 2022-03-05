      program andrew_tt
c
      use andrew_comms
c
      implicit real(8) (a-h,o-z)
c
      character(8) oc,oca(2)
      character(10) units
      integer andrew_code_index
      dimension x(2),y(2)
      logical pair,ocmiss
c
c      Reads in gross travel times and delivers finer times 
c      on basis of lon/lat data 
c
      call andrew_get_outcode_ll
c
      noc=0
c     radius=6378.1D+00 ! Kilometres
c     units ='kilometres'
      radius=3963.2D+00 ! Miles
      units ='miles     '
c
  1   continue
      read(*,*,end=99)oca
      pair=.true.
      do noc=1,2
         nx=andrew_code_index(oca(noc))
c
         if(nx.lt.1.or.
     *      nx.gt.1296
     *     )then
                write(*,104)oca(noc)
                pair=.false.
                exit
         endif
c
         if(outcode_index(nx).eq.0
     *     )then
                write(*,104)oca(noc)
                pair=.false.
                exit
         endif
c
         ocmiss=.true.
         do i=outcode_index(nx),noutrecs
            if(outcode(i).eq.oca(noc)
     *        )then
c                  write(*,100)outcode(i),outcode_index(nx),i
c    *                         ,outlon(i),outlat(i)
                   x(noc)=outlon(i)
                   y(noc)=outlat(i)
                   ocmiss=.false.
                   exit
            endif
         enddo
c
         if(ocmiss
     *     )then
                pair=.false.   ! Found index to 1st two chars, but no outcode..
                write(*,104)oca(noc)
         endif
      enddo
c
      if(pair
     *  )then
             d=sphere_dist(x(1),y(1),x(2),y(2),radius)
             write(*,102)oca,d,units
      endif
c
      go to 1
c
 99   stop
c
100   format(a10,2i6,2f20.14)
102   format('Spherical surface distance between ',a8,' and ',a8,' is'
     *      ,f8.3,' ',a10)
104   format('Invalid outcode: ',a10)
200   format(2(4f30.18,/))
      end
