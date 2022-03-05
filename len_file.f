      integer function len_file(nchann)
c
c      Returns number of records in formatted file
c
      character(1) junk
c
      len_file=0
      rewind(nchann)
  1   continue
      read(nchann,*,end=99,err=98)junk
      len_file=len_file+1
      go to 1
c
  99  continue
      rewind(nchann)
c     write(0,101)nchann,len_file
      return
c
 98   continue
      write(0,100)nchann,len_file
      rewind(nchann)
      return
c
100   format('ERROR reading unit',i3,' at record',i8)
101   format('Records in unit',i3,':',i8)
c
      end
