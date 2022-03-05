      subroutine io_sizer
c
c        Reads an iostat record to see how many disks it has....
c
      use allcomms
c
      integer, dimension (3) :: nsep
        character *9   devtype,devtypb
        character *5   devclass 
        character *1   devdet(9),devtmp
        character *100 datbline
c
        equivalence (datbline,devtype),(datbline,devclass)
     *             ,(datbline,devtypb),(devdet,devtype)
c
c#####################################################################
c
      maxdev_s=0
      ncount=0
      ntty=0
  1   continue
c
      read(nchann,100,end=99,iostat=ios,err=98)datbline
      ncount=ncount+1
c
c      Look for line containing tty to start the record:
c
      if(devtype.ne.'tty:     '.and.
     *   devtype.ne.'avg-cpu: '
     *  )then
c            write(*,104)ncount,devtype
             go to 1
         else
             ntty=ntty+1
             nsep(ntty)=ncount
             if(ntty.lt.3	! Guard against 'no-history' iostat data...
     *         )then
                    go to 1
             endif
      endif
c
      maxdev_s=nsep(3)-nsep(2)-5
      write(*,103)nchann,maxdev_s
c     
      return
c
 98   continue
      write(0,101)nchann
      return
c
 99   continue
      if(maxdev_s.le.0
     *  )then
             write(0,102)
      endif
c
100   format(a100)
101   format('##### ERROR IN IO_SIZER: READING IOSTAT RECORD ON'
     *      ,' CHANNEL',i4,' #####')
102   format('##### ERROR IN IO_SIZER: NO DISKS FOUND #####')
103   format('IO_SIZER: Number of disks on channel',i4,' is',i4)
104   format('##### WARNING IN IO_SIZER: NO START TEXT FOUND #####'
     *,i6,a10)
c
      end
