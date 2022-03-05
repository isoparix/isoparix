      subroutine host_input(indexing)
c
c      Reads the conditioned output from iostat on the hosts
c
      use allcomms
c
      logical dn,indexing
c
      character (10000) incoming_text
c
c     if(end_data
c    *  )then
c            return
c     endif
c
  1   continue
c
c      Get the next iostat record from a host
c
      if(nrecords.le.0
     *  )then
c
c      Get the record from a pipe...
c
c     read(*,200,iostat=ios,end=99)incoming_text
c     write(40,*)trim(incoming_text)
c     call isoflush(40)
c
              hostname='HOST_NOT_FOUND_IN_PIPED_INPUT'
              ndev=-1
              ncore=-1
              read(*,*,iostat=ios,end=99)
     *        hostname,cpuser_s,cpsys_s,cpidle_s,cpiow_s
     *                          ,ndev
     *                                    ,(dev(mx)
     *                                     ,tmact_s(mx)
     *                                     ,tkbpsr(mx)
     *                                     ,tkbpsw(mx)
     *                                     ,tps_s(mx)
     *                                     ,mx=1,ndev)
     *                          ,ncore
     *                                    ,(core(mx)
     *                                     ,coreuser_s(mx)
     *                                     ,coresys_s(mx)
     *                                     ,coreidle_s(mx)
     *                                     ,coreiow_s(mx)
     *                                     ,mx=1,ncore)
c
c
c
c
c                   write( 0,1022)nrecords,trim(hostname)
c    *                  ,cpuser_s,cpsys_s,cpidle_s,cpiow_s,ndev,ncore
c                   write(30,1022)nrecords,trim(hostname)
c    *                  ,cpuser_s,cpsys_s,cpidle_s,cpiow_s,ndev,ncore
c                   write(30,1023)                 (dev(mx)
c    *                                             ,tmact_s(mx)
c    *                                             ,tkbpsr(mx)
c    *                                             ,tkbpsw(mx)
c    *                                             ,tps_s(mx)
c    *                                             ,mx=1,ndev)
c                   write(30,1024)                 (core(mx)
c    *                                             ,coreuser_s(mx)
c    *                                             ,coresys_s(mx)
c    *                                             ,coreidle_s(mx)
c    *                                             ,coreiow_s(mx)
c    *                                             ,mx=1,ncore)
c
c                   call flush(30)
         else
c
c      Get the record from a file
c
             nchann=nchann+2
             if(nchann.gt.maxchann
     *         )then
                    nchann=minchann
             endif
             hostname=filename(nchann)
c            write(6,702)nchann,trim(hostname),end_chann(nchann)
c            call flush(6)
             if(.not.end_chann(nchann)
     *         )then
c
c      ...and take the data straight out of the CONVREC data
c
                    call convrec(hostname,.false.)
             endif
      endif
c
      nline=nline+1
c
c     if(ndev.le.0
c    *  )then
c            if(.not.end_data
c    *         )then
c                   write( 0,1021)nrecords,trim(hostname)
c    *                  ,cpuser_s,cpsys_s,cpidle_s,cpiow_s,ndev
c                   go to 1
c            endif
c            return
c     endif
      nsource=0
c     write(*,113)trim(hostname)
      do n=1,nhosts
c        write(*,114)trim(nodename(n))
         if(nodename(n).eq.hostname
     *     )then
c
c      This host is already known
c
                nsource=n
                host_track(n)=.true.
c
                if(nfirstdev(nsource).eq.-1
     *            )then
c
c      This is the first time we've seen this host - 
c      Measure the length of the names....
c
c      ....hostnames...
c
                       if(len_trim(hostname).gt.maxnam
     *                   )then
                              maxnam=len_trim(hostname)
                              write(*,*)maxnam,hostname
                       endif
c
c      ....disknames...
c

                       do nd=1,ndev
                          if(len_trim(dev(nd)).gt.maxnam
     *                      )then
                                 maxnam=len_trim(dev(nd))
                          endif
                       enddo
c
c      ....corenames...
c

                       do nd=1,ncore
                          if(len_trim(core(nd)).gt.maxnam
     *                      )then
                                 maxnam=len_trim(core(nd))
                          endif
                       enddo
c
c      Index its disks...
c
                       nfirstdev(nsource)=nd_index
                 write(8,107)nd_index,ndev,nsource,nhosts,trim(hostname)
                       call isoflush(8)
                       nd_index=nd_index+ndev
                       ndisks (nsource)=ndev
                       ndevtotal=ndevtotal+ndev
                       if(mdph.lt.ndev)mdph=ndev
c
c      Index its cores...
c
                       nfirstcore(nsource)=nc_index
               write(8,1071)nc_index,ncore,nsource,nhosts,trim(hostname)
                       call isoflush(8)
                       nc_index=nc_index+ncore
                       ncores (nsource)=ncore
                       ncoretotal=ncoretotal+ncore
c
                       nhost_count=nhost_count+1
c
                   else
c
c      If it isn't the first time we've seen it, just check that its
c      disk count hasn't changed....
c
                       if(ndisks(nsource).ne.0.and.
     *                    ndisks(nsource).ne.ndev
     *                   )then
c
c      Number of disks has changed for this host....
c
                              write(0,106)ndisks(nsource),ndev,hostname
                       endif
c
c            ....also, just check that its core count hasn't changed....
c
                       if(ncores(nsource).ne.0.and.
     *                    ncores(nsource).ne.ncore
     *                   )then
c
c      Number of cores has changed for this host....
c
                             write(0,1061)ncores(nsource),ncore,hostname
                       endif
                endif
                incount(nsource)=1+incount(nsource)
         endif
      enddo
c
      if(nsource.eq.0)then
c
c      This host should be known, but isn't...
c
                          write(0,100)trim(hostname)
                          write(0,1022)nrecords,trim(hostname)
     *                          ,cpuser_s,cpsys_s,cpidle_s,cpiow_s,ndev
     *                                                            ,ncore
                          return
      endif
c
c      If this is an indexing call, then read the next host.  Oherwise,
c      return.
c
      if(indexing
     *  )then
             if(nhost_count.lt.nhosts
     *         )then
                    go to 1
                else
                    write(30,1021)nrecords,trim(hostname)
     *                  ,cpuser_s,cpsys_s,cpidle_s,cpiow_s,ndev,ncore
                    write(30,1023)                 (dev(mx)
     *                                             ,tmact_s(mx)
     *                                             ,tkbpsr(mx)
     *                                             ,tkbpsw(mx)
     *                                             ,tps_s(mx)
     *                                             ,mx=1,ndev)
                    write(30,1024)                 (core(mx)
     *                                             ,coreuser_s(mx)
     *                                             ,coresys_s(mx)
     *                                             ,coreidle_s(mx)
     *                                             ,coreiow_s(mx)
     *                                             ,mx=1,ncore)
 
                    call isoflush(30)
                    return
             endif
      endif
c
c      Mark this host as having contributed data in this cycle
c
      no_data(lastcol,nsource)=.false.
c
c      Read all the new data into temp storage, for averaging
c
c
c      Was this a RedHat sourcee
c
      if(cpuser_s.lt.0.
     *  )then
             cpuser_s=cpuser_s+200.
             redhat=.true.
         else
             redhat=.false.
      endif
c
c      Record the singular, _s, data into a temporary average
c
      acpusr (nsource)=cpuser_s+acpusr (nsource)
      acpsys (nsource)=cpsys_s +acpsys (nsource)
      acpiow (nsource)=cpiow_s +acpiow (nsource)
      acpidl (nsource)=cpidle_s+acpidl (nsource)
c
      mx=nfirstdev(nsource)-1
      do nd=1,ndev
c        write(*,*)mx,ndev,nsource,nd
         dskname(mx+nd)=dev(nd)
         adskact(mx+nd)=tmact_s(nd)+adskact(mx+nd)
         akbpsr (mx+nd)=tkbpsr(nd)+akbpsr (mx+nd)
         akbpsw (mx+nd)=tkbpsw(nd)+akbpsw (mx+nd)
         atps   (mx+nd)=tps_s(nd)+atps   (mx+nd)
c        write(*,700)mx,nd,atps(mx+nd),tps_s(nd)
      enddo
c
      mx=nfirstcore(nsource)-1
      do nc=1,ncore
c        write(*,*)mx,ncore,nsource,nc
         corename(mx+nc)=core(nc)
         acoreusr(mx+nc)=coreuser_s(nc)+acoreusr(mx+nc)
         acoresys(mx+nc)=coresys_s (nc)+acoresys(mx+nc)
         acoreidl(mx+nc)=coreidle_s(nc)+acoreidl(mx+nc)
         acoreiow(mx+nc)=coreiow_s (nc)+acoreiow(mx+nc)
      enddo
c
      if(incount(nsource).lt.naverage
     *  )then
             return
      endif
c
c      Write the relevant quantities in to the last time-line
c      Multiply the summed data by raverage and put it in the map
c
      allcpu_map(lastcol,nsource)=(acpusr(nsource)
     *                           +acpsys (nsource))*raverage
      cpuser_map(lastcol,nsource)=acpusr (nsource)*raverage
      cpsys_map (lastcol,nsource)=acpsys (nsource)*raverage
      cpiow_map (lastcol,nsource)=acpiow (nsource)*raverage
      cpidle_map(lastcol,nsource)=acpidl (nsource)*raverage
      akbpsrt_map(lastcol,nsource)=0.
      akbpswt_map(lastcol,nsource)=0.
c
c      Calculate average over all cores for a particular source
c
      do nc=nfirstcore(nsource),nfirstcore(nsource)+ncores(nsource)-1
         coreuser_map(lastcol,nc)=acoreusr(nc)*raverage
         coresys_map (lastcol,nc)=acoresys(nc)*raverage
         coreiow_map (lastcol,nc)=acoreiow(nc)*raverage
         coreidle_map(lastcol,nc)=acoreidl(nc)*raverage
      enddo
c
c      Calculate average over all disks for a particular source
c
      do nd=nfirstdev(nsource),nfirstdev(nsource)+ndisks(nsource)-1
         dskact_map(lastcol,nd)=adskact(nd)*raverage
         atps_map  (lastcol,nd)= atps  (nd)*raverage
         akbpsr_map(lastcol,nd)= akbpsr(nd)*raverage
         akbpsw_map(lastcol,nd)= akbpsw(nd)*raverage
                         dsktmp=akbpsw_map(lastcol,nd)
     *                         +akbpsr_map(lastcol,nd)
         dskbps_map(lastcol,nd)=dsktmp*rdmax(nsource)
         if(atps_map(lastcol,nd).gt.0.
     *     )then
                txntmp=dsktmp/atps_map(lastcol,nd)
            else
                txntmp=0.
         endif
         akbpstxn_map(lastcol,nd)=txntmp*rrmax(nsource)
c
c      Record the highest data rate from any disk....
c
         if(dsktmp.gt.akbps_disk_max(nsource)
     *     )then
                akbps_disk_max(nsource)=dsktmp
                akbps_disk_peak(nsource)=amedian(dsktmp
     *                                         ,disk_med(:,nsource)
     *                                         ,index_disk_low(nsource)
     *                                          )
                if(akbps_disk_peak(nsource).gt.0.
     *            )then
                       rdmax(nsource)=100./akbps_disk_peak(nsource)
                       akbps_disk_rev(nsource)=.true.
                       write(*,111)nsource,akbps_disk_peak(nsource)
                endif
         endif
c
c      Record the highest kilobytes/transaction from any disk....
c
         if(txntmp.gt.akbps_txn_max(nsource)
     *     )then
                akbps_txn_max(nsource)=txntmp
                akbps_txn_peak(nsource)=amedian(txntmp
     *                                         ,txn_med(:,nsource)
     *                                         ,index_txn_low(nsource)
     *                                          )
                if(akbps_txn_peak(nsource).gt.0.
     *            )then
                       rrmax(nsource)=100./akbps_txn_peak(nsource)
                       akbps_txn_rev(nsource)=.true.
                       write(*,112)nsource,akbps_txn_peak(nsource)
                endif
         endif
c
c        write(*,701)lastcol,nd,atps_map(lastcol,nd),atps(nd),raverage
         akbpsrt_map(lastcol,nsource)= akbpsr_map(lastcol,nd)
     *                               +akbpsrt_map(lastcol,nsource)
         akbpswt_map(lastcol,nsource)= akbpsw_map(lastcol,nd)
     *                               +akbpswt_map(lastcol,nsource)
      enddo
c
c      Add this source's qualities to the grand total..
c      ...and note how many hosts contibute...
c
      gallcpu_map(lastcol)= allcpu_map(lastcol,nsource)
     *                    +gallcpu_map(lastcol)  
      gcpuser_map(lastcol)= cpuser_map(lastcol,nsource)
     *                    +gcpuser_map(lastcol)
      gcpsys_map (lastcol)= cpsys_map (lastcol,nsource)
     *                    +gcpsys_map (lastcol)
      gcpiow_map (lastcol)= cpiow_map (lastcol,nsource)
     *                    +gcpiow_map (lastcol)
      gcpidle_map(lastcol)= cpidle_map(lastcol,nsource)
     *                    +gcpidle_map(lastcol)
      gkbpsrt_map(lastcol)=gkbpsrt_map(lastcol)
     *                    +akbpsrt_map(lastcol,nsource)
      gkbpswt_map(lastcol)=gkbpswt_map(lastcol)
     *                    +akbpswt_map(lastcol,nsource)
c
c      Clear all the accumulators for this source...
c
      incount(nsource)=0
      acpusr (nsource)=0.
      acpsys (nsource)=0.
      acpiow (nsource)=0.
      acpidl (nsource)=0.
c
      do nd=nfirstdev(nsource),nfirstdev(nsource)+ndisks(nsource)-1
         adskact(nd)=0.
         akbpsr (nd)=0.
         akbpsw (nd)=0.
         atps   (nd)=0.
      enddo
c
      do nc=nfirstcore(nsource),nfirstcore(nsource)+ncores(nsource)-1
c        write(*,1071)nfirstcore(nsource),ncores(nsource)
c    *               ,nsource,nhosts,trim(hostname)
         acoreusr(nc)=0.
         acoresys(nc)=0.
         acoreidl(nc)=0.
         acoreiow(nc)=0.
      enddo
c
      return
c
 99   continue
c
c      At this point, the input has ended...
c
      write(0,115)
      end_data=.true.
      return
c
100   format('***** WARNING in HOST_INPUT: Could not find host ',a
     *      ,' in expected list')
101   format('Confirmed source',i4,' of',i4,': ',a40)
102   format(a60,4f6.1,i4,a8000)
1021  format('HOST_INPUT: NRECORDS',i6,' ',a
     *      ,' Usr/sys/idl/wait=',4f6.1,', NDEV=',i4,', NCORE=',i4
     *      ,' - skipping this record, reading next')
1022  format('HOST_INPUT: NRECORDS',i6,' ',a
     *      ,' Usr/sys/idl/wait=',4f6.1,', NDEV=',i4,', NCORE=',i4)
1023  format(400(/'Device: ',a15,4f10.1))
1024  format(400(/'  Core: ',a15,4f10.1))
103   format('Source:',i3,', Count:',i3,14f6.1)
104   format(/)
105   format(200('DISK: ',a15,f5.1,2f7.1,/))
106   format('***** WARNING in HOST_INPUT: Number of disks has changed'
     *      ,' from',i4,' to',i4,' on host ',a)
1061  format('***** WARNING in HOST_INPUT: Number of cores has changed'
     *      ,' from',i4,' to',i4,' on host ',a)
107   format('Starting at',i6,':',i4,' disks of source',i4,' of',i4
     *      ,' (',a,')') 
1071   format('Starting at',i6,':',i4,' cores of source',i4,' of',i4
     *      ,' (',a,')') 
108   format('Total disks',i6)
110   format(200(a15,f5.1,2f7.1))
111   format('Max disk rate on source',i4,' is now',f10.1,' KB/S')
112   format('Max KB/txn    on source',i4,' is now',f10.1)
113   format('HOST_INPUT: Trying to match incoming hostname ',a)
114   format('                                   ...against ',a)
115   format('HOST_INPUT: End of pipe...')
116   format('HOST_INPUT: ND=',i6)
200   format(a10000)
700   format('MX=',i4,', ND=',i4,', ATPS(MX+ND)=',f9.1
     *      ,', TEMPDATD(ND)=',f9.1)
701   format('LASTCOL=',i4,', ND=',i4,', ATPS_MAP(LASTCOL,ND)=',f9.1
     *      ,', ATPS(ND)=',f9.1,', RAVERAGE=',f9.1)
702   format('HOST_INPUT: Channel=',i6,', Host is ',a
     *      ,', End-channel: ',l1)
      end
