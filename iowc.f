      program iowc
c
c      Counts disks, records in set of iostat files
c
      maxdisks=0
      maxrecords=0
c
      ndisktot=0
      nrecords=0
      nhosts=0
      missed=3		!Number of non-blank, non-device, lines....
c
  1   continue
      read(*,*,end=99,err=98)nlines
      read(*,*,end=99,err=98)ntty
c
      if(nlines.le.0.or.ntty.le.0
     *  )then
             go to 1
      endif
c
      nhosts=nhosts+1
c
      if(ntty.gt.maxrecords
     *  )then
             maxrecords=ntty
      endif
c
      ndisks=(nlines/ntty)-missed
      nl=(ndisks+missed)*ntty
c     write(*,104)nlines,ntty,nl,ndisks
c
      if(nl.ne.nlines
     *  )then
c
c      we have an incomplete record
c
             ndisks=ndisks+1
c            write(*,105)ndisks
      endif
c
      ndisktot=ndisktot+ndisks
      nrecords=nrecords+ntty
      if(ndisks.gt.maxdisks
     *  )then
             maxdisks=ndisks
c            write(*,106)maxdisks
      endif
      go to 1
c
 98   continue
      write(*,101)
c
 99   continue
c
      if(nrecords.gt.0
     *  )then
             open(30,file='solo_stat.iostat',status='unknown'
     *           ,form='formatted')
             open(32,file= 'all_stat.iostat',status='unknown'
     *           ,form='formatted')
             write(30,102)     1,maxdisks,maxrecords,maxdisks
             write(32,103)nhosts,maxdisks,  nrecords,ndisktot
         else
             write(0,107)
      endif
      stop
c
100   format(i10,' disks in',i10,' records')
101   format('##### INPUT ERROR IN IOWC #####')
102   format(4i10,' N_hosts, Max_disks_per_host, Max_records_per_host,'
     *      ,     ' Max_disks_per_host')
103   format(4i10,' N_hosts, Max_disks_per_host, Total records,'
     *      ,     ' Total disks')
104   format('NLINES:',i8,', NTTY:',i6,', Predicted NLINES:',i8
     *      ,', NDISKS:',i4)
105   format('NDISKS amended to',i4)
106   format('Max disks per hosts now',i4)
107   format(/'##### ERROR IN IOWC #####:  No iostat records found',/)
c
      end
