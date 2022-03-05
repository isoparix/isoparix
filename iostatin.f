      program iostat_int
c
c        Produces a single useful line from iostat readings
c
      use allcomms
c
      parameter (nblock=20,nsamples=5)
c
      real *4 s_akbps(nblock)
c
        character *10  response,out2,out3
        character *9   devtype
        character *8   out1
        character *5   devclass 
        character *60  source_id
        character *1   devdet(9),devtmp
        character *100 datbline
c
        equivalence (datbline,devtype),(datbline,devclass)
     *             ,(devdet,devtype)
c
c#####################################################################
c
      nchann=5
      tcpuser=0.
      tcpsys =0.
      tcpidle=0.
      tcpiow =0.
      tread  =0.
      twrtn  =0.
      nline=0
      navg=1
      do n=1,nblock
         s_akbps(n)=-1
      enddo
c
      write(*,105)nsamples
c
c      How many disks do we have locallY?
c
      call io_sizer
c
      nqsamp      =0
      maxdevs     =0
      maxhosts    =1
      maxtimes    =0
      maxrows     =0
      max_screen_x=0
      max_screen_y=0
c
      call mem_alloc('iostat  ')
c
c
  1   continue
      end_data=.true.
      call read_io_rec
      ndev=ndev_s
c
c****************************************
c
c        All data read for this instance of iostat
c
c      Write out a useful line
c
      if(mod(nline,navg).eq.0
     *  )then
             tot_tps=0.
             tot_akbps=0.
             itot_kbread=0.
             itot_kbwrtn=0.
      endif
c
      do nd=1,ndev
         tot_tps   =tot_tps   +tps_s   (nd)
         tot_akbps =tot_akbps +akbps_s (nd)
         itot_kbread=itot_kbread+kbread_s(nd)
         itot_kbwrtn=itot_kbwrtn+kbwrtn_s(nd)
      enddo
c
c      How many KB read/written in total?
c
      if(itot_kbread+itot_kbwrtn.gt.0
     *  )then
             a=tot_akbps/float(itot_kbread+itot_kbwrtn)
             pd=.01/a
         else
             a=1.
      endif
c
      tot_wkbps=a*float(itot_kbwrtn)
      tot_rkbps=a*float(itot_kbread)
      nline=nline+1
      if(mod(nline,nblock).eq.0
     *  )then
             write(*,105)nsamples
      endif
c
c      Maintain list of the last nblock-1 values of TOT_AKBPS
c
      nsd=0
      do i=nblock,2,-1
        s_akbps(i)=s_akbps(i-1)
        if(s_akbps(i).ge.0.
     *    )then
               nsd=nsd+1
        endif
      enddo
c
c      Add new entry, and calculate means and SDs...
c
      s_akbps(1)=tot_akbps
      nsd=nsd+1
      if(nsd.ge.nsamples
     *  )then
             call sdev(s_akbps,nsamples)
c            write(*,300)nsd,(s_akbps(mx),mx=1,nsamples)
c            write(*,300)nsd,mux,sdx,sdx**2
300   format(i3,20f10.1)
      endif
c 
      if(tot_tps.gt.0.
     *  )then
             write(out1,107)tot_akbps/tot_tps
         else
             write(out1,110)
      endif
c 
      if(mux.gt.0.
     *  )then
             write(out2,109)sdx/mux
         else
             write(out2,111)
      endif
c 
      if(cpuser_s.lt.0.0
     *  )then
             cpuser_s=200.+cpuser_s
      endif
c
      if(cpuser_s+cpsys_s.gt.0.
     *  )then
             write(out3,112)tot_akbps/(cpuser_s+cpsys_s)
         else
             write(out3,111)
      endif
c
      write(*,102 )cpuser_s,cpsys_s,cpidle_s,cpiow_s,tot_tps,tot_akbps
     *                  ,out3,out1,tot_rkbps,tot_wkbps
     *                  ,itot_kbread,itot_kbwrtn,mux,out2
      write(8,1021)cpuser_s,cpsys_s,cpidle_s,cpiow_s,tot_tps,tot_akbps
     *                  ,out3,out1,tot_rkbps,tot_wkbps
     *                  ,itot_kbread,itot_kbwrtn
c
c      Keep and write out running totals
c
      if(pd.gt.0..and.
     *   nline.gt.1
     *  )then
             tcpuser=tcpuser+(cpuser_s*pd)
             tcpsys =tcpsys +(cpsys_s *pd)
             tcpidle=tcpidle+(cpidle_s*pd)
             tcpiow =tcpiow +(cpiow_s *pd)
             tread  =tread+float(itot_kbread)
             twrtn  =twrtn+float(itot_kbwrtn)
c            t      =pd*float(100*nline)
             write(4,113)t,tcpuser,tcpsys,tcpidle,tcpiow,tread,twrtn
      endif
c
c     write(10,107)cpuser_s
c     write(12,107)cpsys_s
c     write(14,107)cpidle_s
c     write(16,107)cpiow_s
c     write(18,107)tot_tps
c     write(20,107)tot_akbps
c     write(22,108)itot_kbread
c     write(24,108)itot_kbwrtn
c
      if(end_data
     *  )then
             stop
         else
             go to 1
      endif
c
c
100   format(a100)
101   format(a85)
102   format(4f6.1,f8.1,f10.1,a10,a8,2f10.1,2i10,f10.1,a10)
1021  format(4(f6.1,','),f8.1,',',f10.1,',',a10,',',a8,','
     *      ,2(f10.1,','),2(i10,','))
103   format('***** ERROR in CONVREC:  Disk',i5
     *      ,' found - max number is',i5)
104   format(1x,a9,85a1)
105   format( '#'
     *      ,/'#                            All       All'
     *      , '                        Read     Write',20x
     *      , '    Last',i3,' All_KBpS'
     *      ,/'# User   Sys  Idle  Wait     tpS      KBpS'
     *      , ' KB/S/%CPU  KB/txn      KBpS      KBpS'
     *       ,'   KB_Read   KB_Writ      Mean   SD/Mean',/'#')
106   format('END_OF_DATA encountered on channel',i4)
107   format(f8.1)
108   format(i10)
109   format(f10.6)
110   format('      - ')
111   format('        - ')
112   format(f10.1)
113   format(5(f8.1,','),2(f20.1,','))
600   format(i3,':',10f7.2)
c
        end
