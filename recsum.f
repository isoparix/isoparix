      program recsum
c
c      Counts records in set of vmstat files
c
      maxdisks=0
      maxrecords=0
c
      read(*,*,end=99,err=98)nlines
      nrecords=nlines		! Records are alrady sorted...
      write(30,102)nlines
c
  1   continue
      read(*,*,end=99,err=98)nlines
      nrecords=nrecords+nlines
      go to 1
c
 98   continue
      write(*,101)
c
 99   continue
c
      write(32,103)nrecords
      stop
c
100   format(i10,' disks in',i10,' records')
101   format('##### INPUT ERROR IN IOWC #####')
102   format(i10,' Max_records_per_host')
103   format(i10,' Total records')
c
      end
