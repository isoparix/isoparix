      subroutine andrew_get_tt
c
      use andrew_comms
c
c      Reads in travel times
c
      implicit real(8) (a-h,o-z)
c
      character (8) oc
      integer andrew_code_index
c
      ttcode_index=0
c
c      Sorted by first then second outcodes
c
c      BS1,BS8,00,09,16,3.5728844
c      BS1,BS9,00,07,15,2.535194496
c      BS1,KT1,01,50,30,113.6711618432
c      BS1,KT10,01,45,08,114.4540895552
c
      nttrecs=len_file(9)
      allocate (ttcodea(nttrecs))
      allocate (ttcodeb(nttrecs))
      allocate (tt(nttrecs))
      allocate (speed(nttrecs))
      allocate (distance(nttrecs))
c
      tt=1           ! These two lines for safety...
      distance=1.0
c
      do n=1,nttrecs
         read(9,*,err=98)ttcodea(n),ttcodeb(n)
     *                  ,nhrs,mins,nsecs,distance(n)
         tt(n)=(nhrs*3600)+(mins*60)+nsecs
c
         nxa=andrew_code_index(ttcodea(n))
         if(nxa.lt.1.or.
     *      nxa.gt.1296
     *     )then
                write(*,104)n
                write(*,105)ttcodea(n),ttcodeb(n)
     *                         ,nhrs,mins,nsecs,distance(n)
                stop
         endif
c
         if(ttcode_index(nxa).eq.0
     *     )then
                ttcode_index(nxa)=n
c               write(*,103)nxa,n,outcode(n)
c    *                     ,outlon(n),outlat(n)
c           else
c               write(*,100)   n,outcode(n)
c    *                     ,outlon(n),outlat(n)
         endif
      enddo
c
      write(*,102)nttrecs
      return
c
 98   continue
      write(*,101)nttrecs
      stop
c
100   format(i16,' ',a8,2f20.14)
101   format('ERROR: reading travel time file after',i8,' records')
102   format('Number of travel time records:',i8)
103   format(2i8,' ',a8,2f20.14)
104   format('Invalid outcode found after',i8,' records')
105   format(2a10,3i4,f10.3)
c
      end
