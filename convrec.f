      subroutine convrec(source_id,single_line_output)
c
c        Produces a single useful line from iostat readings
c
      use allcomms
c
      parameter (len_out=10000)
c
      logical dn,single_line_output
c
        character (10)  response
        character (60)  source_id,srcid_in
        character (15)  devtype
        character (5)   devclass 
        character (1)   devdet(15),devtmp,outarray(len_out),srcid(60)
        character  (len_out) outrec 
        character (100) datbline
c
        equivalence (datbline,devtype),(datbline,devclass)
     *             ,(devdet,devtype),(outrec,outarray)
     *             ,(srcid_in,srcid)
c
c#####################################################################
c
      end_data=.true.
c
c      Clear domain part of source_id
c
      srcid_in=source_id
      dn=.false.
      do i=1,60
         if(srcid(i).eq.'.')dn=.true.
         if(dn)srcid(i)=' '
      enddo
c
      call read_io_rec
      ndev=ndev_s
      if(ndev.le.0
     *  )then
             if(.not.end_data
     *         )then
                    write(*,109)
                    write(0,109)
             endif
             return
      endif
c
c****************************************
c
c       All data read for this instance of iostat
c      Calculate read and write speeds
c
c     write(*,601)nchann,ndev,trim(srcid_in)
      if(ndev.gt.maxdev_s
     *  )then
             ndev=maxdev_s
             write(*,103)maxdev_s,ndev
      endif
c
      do n=1,ndev
         denom=float(kbread_s(n)+kbwrtn_s(n))
         if(denom.gt.0.
     *     )then
c
c      Derive KB/S read and written from absolute KB read/writ
c
                rkbtot=akbps_s(n)/(float(kbread_s(n)+kbwrtn_s(n)))
                tkbpsr(n)=rkbtot*float(kbread_s(n))
                tkbpsw(n)=rkbtot*float(kbwrtn_s(n))
            else
                tkbpsr(n)=0.
                tkbpsw(n)=0.
         endif
      enddo
c
      if(single_line_output
     *  )then
c
c      ...we're running live, so write out a useful line
c
             ndisk_chars=48
             nchars=88+(ndisk_chars*ndev)
c
c      (see format 102...)
c
             if(nchars.gt.len_out
     *         )then
                    ndevnew=(len_out-88)/ndisk_chars
                    write(0,108)nchars,len_out,ndev,ndevnew
                    write(*,108)nchars,len_out,ndev,ndevnew
                    ndev=ndevnew
                    nchars=88+(ndisk_chars*ndev)
             endif
c     
c            write( 0,1021)trim(srcid_in),cpuser_s
c    *                        ,cpsys_s,cpidle_s,cpiow_s,ndev
             write(outrec,102)srcid_in,cpuser_s
     *                        ,cpsys_s,cpidle_s,cpiow_s,ndev
     *           ,(dev(mx),tmact_s(mx),tkbpsr(mx),tkbpsw(mx),tps_s(mx)
     *           ,mx=1,ndev)
             call remblanks(outarray,nchars)
      endif
c
      return
c
100   format(a100)
101   format(a85)
102   format(a60,4f6.1,i4,500(1x,a15,f5.1,3f9.1))
1021  format('CONVREC: ',a,' ',4f6.1,i4)
103   format('***** CONVREC:  Using first',i5,' devices'
     *      ,' found - actual number is',i5)
104   format(1x,a15,85a1)
105   format(a12)
107   format('CONVREC: Output reduced from',i6,' characters to',i6)
108   format('***** ERROR CONVREC: Cannot write',i6,' characters'
     *      ,' in',i6,' buffer.'
     *     ,/'                     Reducing number of disks from'
     *      ,i6,' to',i6)
109   format('***** ERROR CONVREC: '
     *      ,'No devices found - iostat file in error')
110   format('CONVREC_1: AIX=',l1,', REDHAT=',l1)
111   format('CONVREC_2: AIX=',l1,', REDHAT=',l1)
600   format(i3,':',10f7.2)
601   format('CONVREC: NCHANN=',i4,', NDEV=',i4,', SOURCE_ID ',a)
c
        end
