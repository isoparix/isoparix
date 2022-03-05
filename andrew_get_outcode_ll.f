      subroutine andrew_get_outcode_ll
c
      use andrew_comms
c
c      Reads in outcode lon and lat
c
      implicit real(8) (a-h,o-z)
c
      character (8) oc
      integer andrew_code_index
c
      outcode_index=0
c
      noutrecs=len_file(3)
      allocate (outlon(noutrecs))
      allocate (outlat(noutrecs))
      allocate (outcode(noutrecs))
c
      do n=1,noutrecs
         read(3,*,err=98)outcode(n),outlon(n),outlat(n)
c
         nx=andrew_code_index(outcode(n))
         if(nx.lt.1.or.
     *      nx.gt.1296
     *     )then
                write(*,104)n
                stop
         endif
c
         if(outcode_index(nx).eq.0
     *     )then
                outcode_index(nx)=n
c               write(*,103)nx,n,outcode(n)
c    *                  ,outlon(n),outlat(n)
c           else
c               write(*,100)   n,outcode(n)
c    *                  ,outlon(n),outlat(n)
         endif
      enddo
c
      write(*,102)noutrecs
      return
c
 98   continue
      write(*,101)noutrecs
      stop
c
100   format(i16,' ',a8,2f20.14)
101   format('ERROR: reading outcode_lonlat file after',i8,' records')
102   format('Number of lon/lat records:',i8)
103   format(2i8,' ',a8,2f20.14)
104   format('Invalid outcode found after',i8,' records')
c
      end
