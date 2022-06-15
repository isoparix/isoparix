      program colstat
c
c      Get the record from a file
c
c
c      Takes single line of data from multiple streams of iostat data
c      filtered by another program, and draws a line of boxes, 
c      colour-coded for activity levels
c
      use allcomms
c
      integer(4) usleep_
c
      character (60) tempnode
      character ( 1) tempnodea(60)
      character ( 7) avg_txt
c
      equivalence(tempnode,tempnodea)
      equivalence(tempnode(1:7),avg_txt)
c
      real (8) tabs,thstart,thend,ta,tb,tc,td,tidle,tstart
     *        ,thost,thost1,thost2,tdead,t_run,threshold1,threshold2
c
      logical small_screen
      logical inzone
c
      aix=.false.
      redhat=.false.
      winwidth_old=-1
      winheight_old=-1
c
c######################################################################
c#                                                                    #
c#  Collection of all data on sizes, and allocation of memory begins  #
c#                                                                    #
c######################################################################
c
c      Read the size of the problem
c
      open (80,file='all_stat.iostat',err=97)
      read (80,*,err=97)maxhosts,maxdev_s,nqsamp,maxdevs
     *                ,maxcore_s,maxcores
      close(80)
c
c      'interval' is the interval at which colstat expects to 
c       receive data
c      'nrecords' is the number of records that will be read from a 
c       logfile of iostat data and displayed.
c
      read(9,*)interval
      read(9,*)nrecords
      read(9,*)n_cpu_attributes
c
c
c      Read in all the hostnames...
c
      nline=0
      nqsamp=0
      maxnam=0
      nhosts=maxhosts
      nhx=0
      do nh=1,nhosts
         read(3,202,end=96,err=96)tempnode
         if(len_trim(tempnode).gt.maxnam
     *     )then
                maxnam=len_trim(tempnode)
         endif
         nhx=nhx+1
      enddo
      rewind(3)
c
c      Read screen size...
c
      open (30,file='small_screen.dat')
      read (30,*)small_screen
      if(small_screen
     *  )then
             read(30,*)ixsmall
             read(30,*)iysmall
      endif
      close(30)
c
c      Do we have writable display? and how big is it?
c
      call x11dispinfo(%ref(ixmax),%ref(iymax)
     *                  ,%ref(nwhite),%ref(nblack),%ref(irc))
c
      if(irc.lt.0)then
                      write(0,108)
                      stop
      endif
c
      if(small_screen
     *  )then
             if(ixmax.gt.ixsmall
     *         )then
                    ixmax=ixsmall
             endif
c
             if(iymax.gt.iysmall
     *         )then
                    iymax=iysmall
             endif
      endif
c
c      140 is > len('Global total CPU')*8
c
      nhsp=max0(8*(maxnam+1),140)
      nedge=35
      ntimeslots=ixmax-nedge-nhsp-2
c 
c     maxhosts=    maxhosts+1
      maxtimes=    ntimeslots
      maxrows=     6+n_cpu_attributes+max0(maxhosts,maxdev_s)
      max_screen_x=ixmax
      max_screen_y=iymax
      write(*,*)maxhosts,maxdev_s,nqsamp,maxdevs,maxrows
c
      if(nhosts*maxdevs*maxdev_s.gt.0
     *  )then
             write(*,123)nhosts,maxdevs,maxdev_s
         else
             write(0,126)nhosts,maxdevs,maxdev_s
             stop
      endif
c
      call mem_alloc('iostat  ')
c
c
c######################################################################
c#                                                                    #
c#  Collection of all data on sizes, and allocation of memory ends    #
c#                                                                    #
c######################################################################
c
c      Read in all the hostnames...,   again.....
c
      do nh=1,nhosts
         read(3,202,err=98,end=99)tempnode
         nodename(nh)=tempnode
c
c      Pick the ssh name out of the nodename
c
         if(avg_txt.eq.'Average'
     *     )then
                do i=1,48
                   tempnodea(i)=tempnodea(i+12)
                enddo
            else
                do i=1,60
                   if(tempnodea(i).eq.'_'
     *               )then
                          do j=i,60
                             tempnodea(j)=' '
                          enddo
                          exit
                   endif
                enddo
         endif
         ssh_name(nh)=tempnode
         write(*,129)nh,trim(nodename(nh)),trim(ssh_name(nh))
      enddo
c
      tod='               '
      ruler=.false.
      help_displayed=.false.
      open_window=.false.
c
      mdph=0
      fullmap=.false.
c
c      Read date last backed up....
c
      open (30,file='version.date')
      read (30,*)version_date
      close(30)
c
      open (30,file='type.view')
      read (30,*)type_view
      close(30)
c
      title_header=type_view//' - J S WATTS '//version_date
      catch_up=.false.
      allcpu_map=0.
      ixtxt1min=1000000
      ndelta=1
      adelta=1.0
      lmxold=0
      ndy1=0
      nwinpels=0
c
c      Buttons....
c
      ihsepbutt=2
      ivsepbutt=30
      disp_top=2*ivsepbutt
      leftbutt=20
      butt_height=20
      zone_butt=-1
c
      nd_index=1
      nfirstdev=-1
      nc_index=1
      nfirstcore=-1
      nhost_count=0
      ndevtotal=0
      gkbpsrt_map=0.
      gkbpswt_map=0.
      acpusr=  0.
      acpsys=  0.
      acpiow=  0.
      acpidl= 0.
      adiskact=0.
      incount=0
      lastcol=1
      ntold=0.
      nrold=0.
      nrtotal=0.
      nr=0
      host_track=.false.
      numbers=   .false.
      updating=  .false.
      akbps_disk_max=0.
      akbps_txn_max= 0.
      akbps_disk_peak=0.
      akbps_txn_peak= 0.
      akbps_disk_rev=.false.
      akbps_txn_rev= .false.
c
      npslot=1
       txn_med=0.
      disk_med=0.
      index_disk_low=1
      index_txn_low=1
c
c   read(9 statements were after this
c
      idle_sleep=50000
      tidle=.000001*real(idle_sleep)
      tread_min=.1
      threshold1=tidle
      threshold2=threshold1+(2*tidle)
      msleeps=(real(interval)-tread_min)/tidle
      ksleeps=msleeps
c
c      Read from each host, to know about how many disks it has, etc...
c
      if(nrecords.gt.0
     *  )then
c
c      Open all the files in fort.3
c
             minchann=101
             nchann=minchann
             do n=1,nhosts
                filename(nchann)=nodename(n)
                end_chann(nchann)=.false.
c               write(8,200)nchann,n,nodename(n),filename(nchann)
                open(nchann,form='formatted',status='old'
     *              ,file=nodename(n))
                nchann=nchann+2
             enddo
c
  12         continue
             maxchann=nchann-2
c            write(8,201)minchann,maxchann
             nchann=minchann-2
      endif
c
      call host_input(.true.)
c
c      How should we average the data, if it's from a file 
c      (ie nrecords is > 0?
c
      if(nrecords.gt.0
     *  )then
c
c      The data is (are?) coming from file(s)
c
             naverage=1.+(real(nrecords)/real(ntimeslots))
             ixm=nhsp+2+int(.5+((real(nrecords)/real(naverage))))
             ntimeslots=real(nrecords)/real(naverage)
c
             if(ntimeslots.ge.10
     *         )then
                    npslot=ntimeslots/10
             endif
c
             write(*,*)ntimeslots,npslot
         else
             ixm=ixmax-nedge
             naverage=1
      endif
c
c      Make sure the screen is wide enough to show the text in title bar
c
      if(ixm.lt.800.and.ixmax.gt.800
     *  )then
             ixm=800
             ntimeslots=800-nhsp-2
      endif
c
      raverage=1./real(naverage)
      write(*,103 )ntimeslots
      write(8,103 )ntimeslots
      if(nrecords.gt.0
     *  )then
             write(8,1031)nrecords,naverage
      endif
      call isoflush(8) 
c     interval=interval*naverage
c
c      If only one host, then open detail window.  Otherwise show
c      all hosts' CPU together.
c
      global_type=1
      ndisk_view_max=3
      delta_disk_view=.true.
      delta_core_view=.true.
      ndisp_type=1
c
c      Create a coloured activity calibration line
c
      a=100./real(ntimeslots)
      do i=1,ntimeslots
         calibn_map(i)=real(i)*a
      enddo
c
      call select_view
c
      call tim(tstart)
      thost=0.
  1   continue
c
c      Go out and read the data
c
      call tim(thost1)
      call host_input(.false.)
      nr=nr+1
      nrtotal=nrtotal+1
      call tim(thost2)
      tdead=tdead+thost2-thost1
      t_run=thost2-tstart
      thost=thost2-thost1+thost
c
c      Buttons get checked here once per host input
c
      call buttons(.false.)
      if(end_data)then
                      write(*,101)
                      if(nrecords.gt.0
     *                  )then
                             call paint_all
                      endif
  4                   continue
                      call buttons(.true.)
                      go to 4
      endif
c
      call tim(tc)
      if(nr.eq.nhosts*naverage
     *  )then
c
c      When did we do this?
c
             if(nrecords.eq.0
     *         )then
                    call tim(tpaint)
                    ix=tpaint+.5
                    ihrs=ix/3600
                    mins=(ix-(ihrs*3600))/60
                    isecs=ix-(ihrs*3600)-(mins*60)
                    write(tod(lastcol),120)ihrs,mins,isecs
                else
                    if(nrecords.gt.0
     *                )then
                           write(tod(lastcol),121)lastcol*naverage
                       else
                           write(tod(lastcol),121)nline
                    endif
             endif
c
c      nhosts have been read, if this is live.   Not necesarily
c      ALL the hosts, but nhosts - some may have shown up twice..
c      Normalise the global totals
c
             a=1./real(nhosts)
             gallcpu_map(lastcol)=a*gallcpu_map(lastcol)  
             gcpuser_map(lastcol)=a*gcpuser_map(lastcol)
             gcpsys_map (lastcol)=a*gcpsys_map (lastcol)
             gcpiow_map (lastcol)=a*gcpiow_map (lastcol)
             gcpidle_map(lastcol)=a*gcpidle_map(lastcol)
c
c      Paint the current state...
c
             if(           nrecords.le.0.or.
     *                       npslot.eq.1.or.
     *          mod(lastcol,npslot).eq.1
     *         )then
                    call paint_all
                    call buttons(.false.)
             endif
             nr=0
             ntold=nt
             lastcol=lastcol+1
c
c      If we have come to the end of the array, shift everything 
c      backwards, by shfting the pointer
c      update...
c
             if(lastcol.gt.ntimeslots
     *         )then
                    updating=.true. 
                    lastcol=1
             endif
c
             gallcpu_map(lastcol)=0.
             gcpuser_map(lastcol)=0.
             gcpsys_map (lastcol)=0.
             gcpiow_map (lastcol)=0.
             gcpidle_map(lastcol)=0.
             gkbpsrt_map(lastcol)=0.
             gkbpswt_map(lastcol)=0.
c
c      Mark the next slot as invalid - to be changed when host 
c      contributes
c
             do n=1,nhosts
                no_data(lastcol,n)=.true.
             enddo
c
c      Make the amount of time we sleep a function of how long the last
c      host_input took.   Then if we have a rush of inputs we won't
c      delay.
c
             inzone=.true.
             if(tdead.lt.threshold1
     *         )then
                    catch_up=.true.
                    inzone=.false.
                    if(msleeps.ge.ndelta
     *                )then
                           msleeps=msleeps-ndelta
                           ndelta=ndelta*2
                       else
                           msleeps=0
                    endif
             endif
c
             if(tdead.gt.threshold2
     *         )then
                    ndelta=1
                    if(catch_up
     *                )then
c
c      We're coming from a catch-up state...
c
c                          msleeps=ksleeps
                           msleeps=msleeps+ndelta
                       else
                           msleeps=msleeps+1
                    endif
                    catch_up=.false.
                    inzone=.false.
             endif
c
c      Capture a good value...?
c
             if(inzone
     *         )then
                    ndelta=1
                    ksleeps=msleeps
             endif
c
             nsleeps=msleeps
c            write(*,122)t_run,thost,thost/t_run
c    *             ,threshold1,tdead,threshold2,nsleeps,inzone,ksleeps
             tdead=0.
c
             if(nrecords.le.0
     *         )then
c
c      Delay between host read.
c
c                   write(*,801)nsleeps,idle_sleep,nsleeps*idle_sleep
c    *                         ,nrtotal
                    do n=1,nsleeps
                       call buttons(.false.)
                       call micropause(idle_sleep)
                    enddo
             endif
c
      endif
c
      go to 1
c
 96   continue
      write(0,128)nhosts,nhx
      stop
c
 97   continue
      write(0,127)
      stop
c
 98   continue
      write(0,124)nh
      stop
c
 99   continue
      write(0,125)nh
      stop
c
100   format('PAINT_PREP  ARGMT MAP:',50f6.1)
101   format('COLSTAT: END_DATA encountered')
102   format('COLSTAT: Data record:',i8,'. Last column is',i5)
103   format('COLSTAT: Number of time-slots=',i6)
1031  format('            Number of records=',i6
     *     ,/'            Averaging ratio is',i4,':1'
     *     ,/)
104   format(8(a,' '))
105   format('Time-line:',i4,', Time of day:',f10.1,', NT/OLD=',2i6)
108   format('STOP after x11_disp_info failure')
109   format('Painted at',f8.1)
110   format('Painted at:',f8.1,' Buttons checked:',f8.1
     *      ,': Idle elapsed=',f8.1,', max=',f8.1)
111   format('Button check from paint at',f8.1)
112   format('About to read iostat data at',f8.1)
113   format(39x,'Catching up - TDUE',f10.3,' TABS',f10.3)
114   format('Host_input:',f10.3,f7.4,f10.3
     *      ,' Now',f10.3,' TDUE',f10.3
     *      ,' TABS',f10.3,' Sleep cycs',f6.1
     *      ,' Surveys',i3,f6.0,' mS')
115   format('TestHost',i3.3)
116   format('COLSTAT:',i4,' is the maximum number of hosts'
     *      ,' permitted - ignoring others')
120   format(i9,':',i2.2,':',i2.2)
121   format('Interval:',i6)
122   format('Run=',f6.1,', All host-time=',f7.3,', ratio=',f7.5
     *      ,': Thresh1/HOST_INPUT/Thresh2',3f7.4
     *      ,' - nsleeps=',i3,l2,', ksleeps=',i3)
123   format('About to allocate space for',i4,' hosts and',i7,' disks'
     *      ,' - max disks per host=',i5)
124   format('##### ERROR IOSTAT.F: READING NAME OF HOST',i4)
125   format('##### ERROR IOSTAT.F: EOF WHEN READING NAME OF HOST',i4)
126   format('##### ERROR in COLSTAT: Cannot allocate space for',i4
     *      ,' hosts and',i7,' disks - max disks per host=',i5)
127   format('##### ERROR in COLSTAT: Cannot open all_stat.iostat')
128   format('##### ERROR in COLSTAT: Tried to read',i4,' hosts but',
     *       ' only found',i4)
129   format(i4,2(' ',a))
c
200   format('  NCHANN=',i4,', N=',i3,' NODENAME(N)= ',a60
     *                               ,' FILENAME(NCHANN)=',a60)
201   format('MINCHANN=',i4,', MAXCHANN=',i4)
202   format(a60)
203   format(i8,4x,a60)
c
801   format('Will sleep',i3,' times for',i7,' microseconds, giving'
     *      ,' total of',i9,' microseconds.  Hosts read:',i3)
c
      end
