      subroutine read_io_rec
c
c        Reads an iostat record...
c
      use allcomms
c
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
      read_error=.false.
      eof_found=.false.
      ndev_s=0
c
  1   continue
c
      read(nchann,100,end=99,iostat=ios,err=98)datbline
c     write(0,800)datbline
800   format('READ_IO_REC:datbline=',a100)
c
c      Look for an EOF line...
c
      if(devtypb.eq.'EndofFile'
     *  )then
             eof_found=.true.
             return
      endif
c
c      Look for line containing tty to start the record:
c
      if(devtype.ne.'tty:     '.and.
     *   devtype.ne.'avg-cpu: '
     *  )then
             go to 1
         else
c            write(0,108)devtype
             if(devtype.eq.'tty:     '
     *         )then
c                   write(0,1081)
                    aix=.true.
             endif
c
             if(devtype.eq.'avg-cpu: '
     *         )then
c                   write(0,1082)
                    redhat=.true.
             endif
c
c      Read the CPU data line (ie, the next one...)
c
             read(nchann,100,end=99,iostat=ios,err=98)datbline
c     write(0,800)datbline
c
c      Look for an EOF line...
c
             if(devtypb.eq.'EndofFile'
     *         )then
                    eof_found=.true.
                    return
             endif
c
             if(aix
     *         )then
                    read(datbline,*)tin_s,tout_s,cpuser_s,cpsys_s
     *                                          ,cpidle_s,cpiow_s
             endif
c
             if(redhat
     *         )then
                    read(datbline,*)cpuser_s,cpnice_s,cpsys_s,cpiow_s
     *                             ,cpidle_s
                    cpuser_s=cpuser_s-200.
             endif
c
c     write(0,800)datbline
      endif
c
c       Look for disk data...
c
  3   continue
      read(nchann,100,end=99,err=98)datbline
c     write(0,800)datbline
c
c      Look for an EOF line...
c
      if(devtypb.eq.'EndofFile'
     *  )then
             eof_found=.true.
             return
      endif
c
c      Look for start of disk data
c
      if(devtype.ne.'Disks:   '.and.
     *   devtype.ne.'Device:  '
     *  )then
             go to 3
      endif
c
  4   continue   
      read(nchann,100,end=99,err=98)datbline
c     write(0,800)datbline
c
c      Look for an EOF line...
c
      if(devtypb.eq.'EndofFile'
     *  )then
             eof_found=.true.
             return
      endif
c 
c      Is this an hdisk? 
c
      if(devdet(1).ne.' '
     *  )then
             ndev_s=ndev_s+1
             if(ndev_s.gt.maxdev_s
     *         )then
c
c      We have more devices than we can cope with
c
                    write(*,103)ndev_s,maxdev_s
                    write(0,103)ndev_s,maxdev_s
                    read_error=.true.
                    go to 4
             endif
c
             dev(ndev_s)=devtype
c
             if(aix
     *         )then
                    read(datbline,*,iostat=irc)devtmp,tmact_s(ndev_s)
     *                                 ,akbps_s(ndev_s),tps_s(ndev_s)
     *                             ,kbread_s(ndev_s),kbwrtn_s(ndev_s)
             endif
c
             if(redhat
     *         )then
                    tmact_s(ndev_s)=0.
                    read(datbline,*,iostat=irc)devtmp
     *                             ,   tps_s(ndev_s)
     *                             , rkbps_s(ndev_s), wkbps_s(ndev_s)
     *                             ,kbread_s(ndev_s),kbwrtn_s(ndev_s)
                    akbps_s(ndev_s)= rkbps_s(ndev_s)+ wkbps_s(ndev_s)
c
c                   write(0,801)devtmp
c    *                             ,   tps_s(ndev_s)
c    *                             , rkbps_s(ndev_s), wkbps_s(ndev_s)
c    *                             ,kbread_s(ndev_s),kbwrtn_s(ndev_s)
c    *                             , akbps_s(ndev_s)
801   format('READ_IO_REC disk data: ',a1,3f10.2,2i10,f10.2)
             endif
c
             if(intsecs.gt.0
     *         )then
c
c      Check sample periods....
c
                    if(akbps_s(ndev_s).gt.0.
     *                )then
                           aperiod=(kbread_s(ndev_s)+kbwrtn_s(ndev_s))
     *                              /akbps_s(ndev_s)
                       else
                           aperiod=float(intsecs)
                    endif
c
c      Reasonable?
c
                    if(aperiod.gt.ahi.or.aperiod.lt.alo.or.
     *                 akbps_s(ndev_s).gt.100000..or.
     *                 tmact_s(ndev_s).gt.101.0
     *                )then
c
c      ....probably we have junk lines in iostat.   Set everything to 
c      zero, and keep the record, so we don't confuse the timing.
c
                            write(0,107)aperiod,devtype, tmact_s(ndev_s)
     *                                 ,akbps_s(ndev_s),kbread_s(ndev_s)
     *                                                 ,kbwrtn_s(ndev_s)
                            tmact_s(ndev_s)=0.
                            akbps_s(ndev_s)=0.
                           kbread_s(ndev_s)=0 
                           kbwrtn_s(ndev_s)=0 
                    endif
             endif
             go to 4
      endif
c
c****************************************
c
c        All data read for this instance of iostat
c
      end_data=.false.
c
c     write(*,104)ndev_s,cpuser_s,cpsys_s,cpidle_s,cpiow_s
c    *           ,(dev(mx),mx=1,ndev_s)
      return
c
 98   continue
c
c      Deal with line in error
c
      write(*,102)
      write(*,*)datbline
c
      write(0,102)
      write(0,*)datbline
c
      read_error=.true.
c
      return
c
 99   continue
c
c      EOF....
c
      if(allocated(end_chann)
     *  )then
             end_chann(nchann)=.true.
             write(*,106)nchann
             end_data=.true.
      endif
      return
c
100   format(a100)
101   format('##### READ_IO_REC: ABOUT TO READ FROM CHANNEL',i3)
102   format('##### ERROR in READ_IO_REC:  read error in -')
103   format('##### ERROR in READ_IO_REC:  Device',i5
     *      ,' found - max number is',i5)
104   format('NDEV=',i4,', CPU:',4f8.1,', DISKS:',100a10)
106   format('##### READ_IO_REC: END_OF_DATA encountered on channel'
     *      ,i4)
107   format(f12.1,' ',a15,2f12.1,2i12)
108   format('READ_IO_REC: DEVTYPE=',a,'=')
1081  format('READ_IO_REC:  This is AIX iostat data')
1082  format('READ_IO_REC:  This is RedHat iostat data')
c
        end
