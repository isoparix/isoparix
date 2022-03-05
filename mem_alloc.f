      subroutine mem_alloc(util_name)
c
      use allcomms
c
      character (8) util_name
c
c      Write out what was received....
c
      write(8,100)util_name,nqsamp,maxdevs,maxdev_s,maxcores,maxcore_s
     *           ,maxhosts,maxtimes,maxrows,max_screen_x,max_screen_y
c
      if(nqsamp.gt.0
     *  )then
c
             allocate (util   (86400))
             allocate (utilmax(86400))
             allocate (utilmin(86400))
             allocate (mapmin (86400))
             allocate (map    (86400))
             allocate (mapmax (86400))
             allocate (mapsum (86400))
             allocate (jutl   (86400))
             allocate (kutl   (86400))
             allocate (itmap  (86400))
c
             allocate (period0(nqsamp))
             allocate (t      (nqsamp))
             allocate (x      (nqsamp))
             allocate (y      (nqsamp))
             allocate (xdata  (nqsamp))
             allocate (ydata  (nqsamp))
             allocate (zdata  (nqsamp))
             allocate (idata  (nqsamp))
             allocate (jdata  (nqsamp))
             allocate (kdata  (nqsamp))
c
             if(util_name.eq.'iostat  '.or.
     *          util_name.eq.'vmstat  '
     *         )then
                    allocate (cpuser (nqsamp))
                    allocate (cpsys  (nqsamp))
                    allocate (cpidle (nqsamp))
                    allocate (cpiow  (nqsamp))
             endif
c
             if(util_name.eq.'netstat '
     *         )then
                    allocate (priminpak(nqsamp))
                    allocate (priminerr(nqsamp))
                    allocate (primexpak(nqsamp))
                    allocate (primexerr(nqsamp))
                    allocate (primcoll (nqsamp))
                    allocate ( totinpak(nqsamp))
                    allocate ( totinerr(nqsamp))
                    allocate ( totexpak(nqsamp))
                    allocate ( totexerr(nqsamp))
                    allocate ( totcoll (nqsamp))
             endif
c
c            if(util_name.eq.'iostat  '
c    *         )then
c                   allocate (tin (nqsamp))
c                   allocate (tout(nqsamp))
c            endif
c
             if(util_name.eq.'vmstat  '
     *         )then
                    allocate (  r(nqsamp))
                    allocate (  b(nqsamp))
                    allocate (avm(nqsamp))
                    allocate (fre(nqsamp))
                    allocate ( re(nqsamp))
                    allocate ( pi(nqsamp))
                    allocate ( po(nqsamp))
                    allocate ( fr(nqsamp))
                    allocate ( sr(nqsamp))
                    allocate ( cy(nqsamp))
                    allocate ( in(nqsamp))
                    allocate (syf(nqsamp))
                    allocate ( cs(nqsamp))
             endif
c
             if(maxdev_s.gt.0
     *         )then
                    allocate (akbps (nqsamp,maxdev_s))
                    allocate (tps   (nqsamp,maxdev_s))
                    allocate (kbread(nqsamp,maxdev_s))
                    allocate (kbwrtn(nqsamp,maxdev_s))
                    allocate (tmact (nqsamp,maxdev_s))
             endif
c
      endif
c
      if(maxhosts.gt.0
     *  )then
             allocate (acpusr        (maxhosts))
             allocate (acpsys        (maxhosts))
             allocate (acpiow        (maxhosts))
             allocate (acpidl        (maxhosts))
             allocate (host_track    (maxhosts))
             allocate (ndisks        (maxhosts))
             allocate (ncores        (maxhosts))
             allocate (incount       (maxhosts))
             allocate (nfirstdev     (maxhosts))
             allocate (nfirstcore    (maxhosts))
             allocate (nodename      (maxhosts))
             allocate (ssh_name      (maxhosts))
             allocate (rdmax         (maxhosts))
             allocate (rrmax         (maxhosts))
c
             allocate (akbps_disk_peak(maxhosts))
             allocate (akbps_disk_rev (maxhosts))
             allocate (akbps_disk_max (maxhosts))
             allocate (akbps_txn_peak (maxhosts))
             allocate (akbps_txn_rev  (maxhosts))
             allocate (akbps_txn_max  (maxhosts))
c
             allocate (index_disk_low (maxhosts))
             allocate (index_txn_low  (maxhosts))
c
             allocate (filename (101:101+(2*maxhosts)))
             allocate (end_chann(    100+(2*maxhosts)))
c
             lenmed=(maxtimes*maxdev_s)/50
             if(lenmed.gt.0
     *         )then
                    allocate ( txn_med(lenmed,maxhosts))
                    allocate (disk_med(lenmed,maxhosts))
             endif
      endif
c
      if(maxtimes.gt.0
     *  )then
             allocate (calibn_map (0:maxtimes))
             allocate (gallcpu_map(0:maxtimes))
             allocate (gcpuser_map(0:maxtimes))
             allocate (gcpsys_map (0:maxtimes))
             allocate (gcpiow_map (0:maxtimes))
             allocate (gcpidle_map(0:maxtimes))
             allocate (gkbpsrt_map(0:maxtimes))
             allocate (gkbpswt_map(0:maxtimes))
             allocate (ncol_map   (0:maxtimes))
             allocate (number_map (0:maxtimes))
             allocate (line_times (0:maxtimes))
             allocate (ncol_delta (0:maxtimes))
             allocate (tod        (0:maxtimes))
c
             if(maxrows.gt.0
     *         )then
                    allocate (hist_map   (0:maxtimes,maxrows))
             endif
c
             if(maxhosts.gt.0
     *         )then
                    allocate (no_data    (0:maxtimes,maxhosts))
                    allocate (allcpu_map (0:maxtimes,maxhosts))
                    allocate (cpuser_map (0:maxtimes,maxhosts))
                    allocate (cpsys_map  (0:maxtimes,maxhosts))
                    allocate (cpiow_map  (0:maxtimes,maxhosts))
                    allocate (cpidle_map (0:maxtimes,maxhosts))
                    allocate (akbpsrt_map(0:maxtimes,maxhosts))
                    allocate (akbpswt_map(0:maxtimes,maxhosts))
             endif
c
             if(maxdevs.gt.0
     *         )then
                    allocate (dskact_map  (0:maxtimes,maxdevs))
                    allocate (dskbps_map  (0:maxtimes,maxdevs))
                    allocate (akbpstxn_map(0:maxtimes,maxdevs))
                    allocate (akbpsr_map  (0:maxtimes,maxdevs))
                    allocate (akbpsw_map  (0:maxtimes,maxdevs))
                    allocate (atps_map    (0:maxtimes,maxdevs))
             endif
c
             if(maxcores.gt.0
     *         )then
                    allocate (coreuser_map(0:maxtimes,maxcores))
                    allocate (coresys_map (0:maxtimes,maxcores))
                    allocate (coreidle_map(0:maxtimes,maxcores))
                    allocate (coreiow_map (0:maxtimes,maxcores))
             endif
      endif
c
      if(maxdevs.gt.0
     *  )then
             allocate (dskname (maxdevs))
             allocate (tempdisk(maxdevs))
             allocate (adskact (maxdevs))
             allocate (akbpsr  (maxdevs))
             allocate (akbpsw  (maxdevs))
             allocate (atps    (maxdevs))
      endif
c
      if(maxdev_s.gt.0
     *  )then
             allocate (akbps_s (maxdev_s))
             allocate (rkbps_s (maxdev_s))
             allocate (wkbps_s (maxdev_s))
             allocate (tps_s   (maxdev_s))
             allocate (kbread_s(maxdev_s))
             allocate (kbwrtn_s(maxdev_s))
             allocate (tkbpsr  (maxdev_s))
             allocate (tkbpsw  (maxdev_s))
             allocate (tmact_s (maxdev_s))
             allocate (dev     (maxdev_s))
      endif
c
      if(maxcores.gt.0
     *  )then
             allocate (coreuser(maxcores))
             allocate (coresys (maxcores))
             allocate (coreidle(maxcores))
             allocate (coreiow (maxcores))
             allocate (corename(maxcores))
             allocate (acoreusr(maxcores))
             allocate (acoresys(maxcores))
             allocate (acoreidl(maxcores))
             allocate (acoreiow(maxcores))
      endif
c
      if(maxcore_s.gt.0
     *  )then
             allocate (coreuser_s(maxcore_s))
             allocate (coresys_s (maxcore_s))
             allocate (coreidle_s(maxcore_s))
             allocate (coreiow_s (maxcore_s))
             allocate (core      (maxcore_s))
      endif
c
      if(maxrows.gt.0
     *  )then
             allocate (absent_host(maxrows))
             allocate (inverted_name(maxrows))
             allocate (row_name(maxrows))
      endif
c
      if(max_screen_x.gt.0
     *  )then
             allocate (line_x     (0:max_screen_x))
      endif
c
      if(max_screen_y.gt.0
     *  )then
             allocate (line_y     (0:max_screen_y))
             allocate (line_host  (0:max_screen_y))
      endif
c
      if(max_screen_x.gt.0.and.max_screen_y.gt.0
     *  )then
             allocate (zone_butt(0:max_screen_x,0:max_screen_y))
      endif
c
c
c
      return
c
100   format('Memory allocation parameters:'
     *     ,/'     Utility name: ',a
     *     ,/'Number of samples:',i10
     *     ,/'          MAXDEVS:',i10
     *     ,/'         MAXDEV_S:',i10
     *     ,/'         MAXCORES:',i10
     *     ,/'        MAXCORE_S:',i10
     *     ,/'         MAXHOSTS:',i10
     *     ,/'         MAXTIMES:',i10
     *     ,/'          MAXROWS:',i10
     *     ,/'     MAX_SCREEN_X:',i10
     *     ,/'     MAX_SCREEN_Y:',i10
     *      )
101   format('##### ERROR in STAT_MODULE: '
     *     , 'Cannot allocate space for median data'
     *     ,/'         MAXDEV_S:',i10
     *     ,/'         MAXTIMES:',i10
     *     ,/'           LENMED:',i10
     *      )
c
      end
