      program iostanal
c
      use allcomms
c
c        Analyses unix iostat data
c
      logical ndevok,periodok
c
        character (15) devtype
        character (9) devtypb
        character (5) devclass 
        character (4) classid 
        character (3) day     
        character (1)  devdet(15),devtmp
        character (100) datbline
c
        equivalence (datbline,devtype)
        equivalence (datbline,devtypb)
        equivalence (devdet,devtype)
        equivalence (datbline,devclass)
c
c
c       version=' IOSTANAL Version 1.2 July 1994'
c       version=' IOSTANAL Version 1.3 February 1996'
c       version=' IOSTANAL Version 1.4 12 March 1996'
c       version=' IOSTANAL Version 1.5 26 April 1996'
c       version=' IOSTANAL Version 1.6  9 May   1997'
c       version=' IOSTANAL Version 1.7 14 June  1997'
c       version=' IOSTANAL Version 1.8 27 June  1997'
c       version=' IOSTANAL Version 1.9 16 July  1997'
c       version=' IOSTANAL Version 2.0 17 July  1997'
c       version=' IOSTANAL Version 2.1 20 October 1997'
c       version=' IOSTANAL Version 2.3 25 November 1997'
c       version=' IOSTANAL Version 2.4  1 December 1997'
c       version=' IOSTANAL Version 2.5 10 February 1998'
c       version=' IOSTANAL Version 3.0 21 April 1998'
c       version=' IOSTANAL Version 3.1 10 May 1998'
c       version=' IOSTANAL Version 3.2 30 September 1998'
c       version=' IOSTANAL Version 3.3 22 November 1998'
c       version=' IOSTANAL Version 3.4 25 November 1998'
c       version=' IOSTANAL Version 3.5  1 December 1998'
c       version=' IOSTANAL Version 3.6 18 January 1999' 
c       version=' IOSTANAL Version 3.7 24 March 2000'   
c       version=' IOSTANAL Version 3.8 26 March 2000'   
c       version=' IOSTANAL Version 3.9 30 March 2000'   
c       version=' IOSTANAL Version 3.91 12 December 2000'
c       version=' IOSTANAL Version 4.00 5 January 2001'
c       version=' IOSTANAL Version 4.01 16 January 2001'
c       version=' IOSTANAL Version 4.02 25th June 2001'
c       version=' IOSTANAL Version 4.03 56h May 2003'
        version=' IOSTANAL Version 4.04 8th July 2003'
c
c      Read the size of the problem
c
      read(5,*,end=99)maxhosts,maxdev_s,nqsamp,maxdevs
      maxtimes    =0
      maxrows     =0
      max_screen_x=0
      max_screen_y=0
c
      call mem_alloc('iostat  ')
c
c############ Read the iostat period, and the averaging quantity ###
c
        read (5,*,end=99)sysold,intsecs,nq,it
c
c      Open all the files
c
      call open_files
c
c      Initialise all the disk data fields...
c
      akbps =0.
      tps   =0.
      kbread=0.
      kbwrtn=0.
      tmact =0.
c
        tolerance=1.3
        ahi=float(intsecs)*tolerance
        alo=float(intsecs)/tolerance
c
        id_err_rec=-999
         n_err_rec=0
c
        write(10,110)sysold,version
        write( 8,113)sysold,version
        write(12,113)sysold,version
        write(14,113)sysold,version
c
        lsqcount=0
        npt=0
        pt=0.
        ptmin=huge(ptmin)
        ptmax=0.
c
        write(14,112)
        ihrs=it/100
        imins=it-(ihrs*100)
        navg=nq*intsecs
c
c      Make tainit an integer number of averaging periods 
c
        ik=(ihrs*3600)+(imins*60)
        tainit=(ik/navg)*navg
c
c#####################################################################
c
c
      nline=0
      maxta=0
      nerrors=0
      nchann=5
c
  2   continue
      ta=tainit
      ntty=0
c
c      Make sure that data lines start on an averaging boundary
c
      nline=(nline/nq)*nq
c
  1   continue
c
      call read_io_rec
      if(eof_found
     *  )then
             go to 2
      endif
c
      if(end_data
     *  )then
             go to 99
      endif
c
c      Record read...
c
      if(nline.ge.nqsamp
     *  )then
             write(*,116)
             go to 99
      endif
      nline=nline+1
      t(nline)=ta
      period0(nline)=float(intsecs)
c
c       Set timing information....
c
      ta=ta+float(intsecs)
      ta=amod(ta,.864e+05)
      if(int(ta).gt.maxta)maxta=ta
c
c     write(*,103)nline,ndev_s,cpuser_s,cpsys_s,cpiow_s,cpidle_s
c
c      Was this a RedHat source?
c
      if(cpuser_s.lt.0.
     *  )then
             cpuser_s=cpuser_s+200.
             redhat=.true.
         else
             redhat=.false.
      endif
      cpuser(nline)=cpuser_s
      cpsys (nline)=cpsys_s
      cpiow (nline)=cpiow_s
      cpidle(nline)=cpidle_s
c
      ndev=ndev_s
      do n=1,ndev
c         write(*,1031)nline,tmact_s(n),akbps_s(n),tps_s(n)
c    *                     ,kbread_s(n),kbwrtn_s(n)
          tmact(nline,n)= tmact_s(n)
          akbps(nline,n)= akbps_s(n)
            tps(nline,n)=   tps_s(n)
         kbread(nline,n)=kbread_s(n)
         kbwrtn(nline,n)=kbwrtn_s(n)
      enddo
c
      if(a.lt.ptmin)ptmin=a
      if(a.gt.ptmax)ptmax=a
      pt=pt+a
      npt=npt+1
c
      go to 1
c
c****************************************
c
c        All data read for this instance of iostat
c
 99   continue
c
c        All data read
c
        close(3)
c       nline=nline-1
        if(nline.le.0
     *    )then
               write(*,126)
               write(0,126)
               stop
           else
               write(*,106)n_err_rec,nline,ptmax,pt/float(npt),ptmin
        endif
c
c     write(*,300)(t(mx),period0(mx),mx=1,30)
300   format(2e20.5)  
        k=nline
        l=ndev
        if(ndev.lt.ndevmax)then
                               write(*,125)ndev,ndevmax
                               ndev=ndevmax
        endif  
c       stop
c      ****************** CORRELATIONS - NO AVERAGES **************
c
c      Find correlation of total CPU and user/system CPU           
c
         xname='Total percentage CPU         '
         do i=1,nline
            x(i)=cpuser(i)+cpsys(i)
         enddo
c
         yname='Percentage user CPU  '
         call lsquare(x,cpuser,-nline)
         write(8,121)xname
         call predict(mux,sdx,1,nline)
         write(8,121)yname
         call predict(muy,sdy,1,nline)
c
         yname='Percentage system CPU  '
         call lsquare(x,cpsys,-nline)
         write(8,121)yname
         call predict(muy,sdy,1,nline)
c
c      Find correlation of user CPU and system CPU           
c
         xname='Percentage user   CPU  '
         yname='Percentage system CPU  '
         call lsquare(cpuser,cpsys,nline)
c
c      Find correlation of total/user/system CPU and total KB/S           
c
         xname='Total percentage CPU         '
         yname='Total Kilobytes/second       '
         do i=1,nline
            y(i)=0.
            do j=1,ndev
               y(i)=y(i)+akbps(i,j)
            enddo
         enddo
         call lsquare(x,     y,-nline)
         write(8,121)yname
         call predict(muy,sdy,1,nline)
c
         xname=yname
c
         xname='Percentage user CPU  '
         call lsquare(cpuser,y,-nline)
c
         xname='Percentage system CPU  '
         call lsquare(cpsys ,y,-nline)
c
c      TEST---------
c
      xname='Total percentage CPU         '
      yname='Total CPU wait+idle (CHECK!!)'
      do i=1,nline
         y(i)=cpiow(i)+cpidle(i)
      enddo        
      call lsquare(x,y,nline)
c
c      Compare KB/S read and KB/S written over all devices
c
         xname='Total Kilobytes/second read  '
         yname='Total Kilobytes/second wrtn  '
         do i=1,nline
            x(i)=0.
            y(i)=0.
            do j=1,ndev
               x(i)=x(i)+float(kbread(i,j))
               y(i)=y(i)+float(kbwrtn(i,j))
            enddo
            x(i)=x(i)/period0(i)
            y(i)=y(i)/period0(i)
c           write(60,700)i,x(i),y(i)
700   format('Line:',i8,', KB/S read:',f20.3,', KB/S written',f20.3)
            call isoflush(60)
         enddo
         call lsquare(x,y,nline)
         write(8,121)xname
         call predict(mux,sdx,1,nline)
         write(8,121)yname
         call predict(muy,sdy,1,nline)
c
         xname=yname
c
c      Find 'percent time active' correlations...
c
         do j=1,ndev
c
            do i=1,nline
               x(i)=tmact(i,j)
            enddo
c
c      Find correlation of CP I/O wait and time active
c
            xname='% time active  '//dev(j)
            yname='% CPU I/O wait '//dev(j)
            call lsquare(x,cpiow,nline)
c
         enddo
c
         write(14,119)
c
         do j=1,ndev
c
            do i=1,nline
               x(i)=tmact(i,j)
               y(i)=akbps(i,j)
            enddo
c
c      Find correlation of data transferred and time active
c
            xname='Percent time active '//dev(j)
            yname='Kilobytes/sec (dev= '//dev(j)//')'
            call lsquare(x,y,nline)
c
         enddo
c
c      *********** END OF CORRELATIONS - NO AVERAGES **************
c
c
c      Start the daily charts
c
c
         call summer(period0,nsamples,0)
c
c      Plot overall system characteristics
c
         call add_toc_item(2,
     *  'Overall system characteristics                  ')
         write(12,111)
c
      title=' Total_CPU (user+system)'
      do i=1,nline
         x(i)=cpuser(i)+cpsys(i)
         if(x(i).gt.100.)then
                             write(*,200)i,x(i),cpuser(i),cpsys(i)
                             x(i)=100.
         endif
      enddo
      if(nq.gt.1)call summer(x,nsamples,1)
      call plotutil(x,t,period0,nsamples)
c
      title=' KiloBytes_per_second_read from all devices '
      do i=1,nline
         y(i)=0.
         do j=1,ndev
            y(i)=y(i)+float(kbread(i,j))
         enddo
         y(i)=y(i)/float(intsecs)
      enddo
      if(nq.gt.1)call summer(y,nsamples,1)
      call plotutil(y,t,period0,nsamples)
c
      title=' KiloBytes_per_second_written to all devices '
      do i=1,nline
         y(i)=0.
         do j=1,ndev
            y(i)=y(i)+float(kbwrtn(i,j))
         enddo
         y(i)=y(i)/float(intsecs)
      enddo
      if(nq.gt.1)call summer(y,nsamples,1)
      call plotutil(y,t,period0,nsamples)
c
      title=' Total_KiloBytes_per_second over all devices '
      do i=1,nline
         y(i)=0.
         do j=1,ndev
            y(i)=y(i)+akbps(i,j)
         enddo
      enddo
c
      if(nq.gt.1)call summer(y,nsamples,1)
      call plotutil(y,t,period0,nsamples)
c
c *********** TEST ***********
c     return
c *********** TEST ***********
c
      title=' KBpS-percent_CPU_ratio'
      do i=1,nline
         if(x(i).gt.0)then
                          y(i)=y(i)/x(i)
                      else
                          y(i)=0.
         endif
      enddo
      call plotutil(y,t,period0,nsamples)
c
      title=' Total_tps over all devices '
      do i=1,nline
         x(i)=0.
         do j=1,ndev
            x(i)=x(i)+tps(i,j)
         enddo
      enddo
      call plotutil(x,t,period0,nsamples)
c
      title=' Kilobytes/transaction over all devices'
      do i=1,nline
         if(x(i).gt.0)then
                         y(i)=y(i)/x(i)
                      else
                          y(i)=0.
         endif
      enddo
      call plotutil(y,t,period0,nsamples)
c
c      Plot individual CPU characteristics
c
      call add_toc_item(2
     *              ,'Particular CPU states                           ')
c
      title=' CPU_user'
      if(nq.gt.1)call summer(cpuser,nsamples,1)
      call plotutil(cpuser,t,period0,nsamples)
c
      title=' CPU_system'
      if(nq.gt.1)call summer(cpsys,nsamples,1)
      call plotutil(cpsys,t,period0,nsamples)
c
      title='CPU_idle'
      if(nq.gt.1)call summer(cpidle,nsamples,1)
      call plotutil(cpidle,t,period0,nsamples)
c
      title='CPU_IO_wait_state'
      if(nq.gt.1)call summer(cpiow,nsamples,1)
      call plotutil(cpiow,t,period0,nsamples)
c
      call add_toc_item(2
     *              ,'Individual device details                       ')
c
      rintsecs=1./float(intsecs)
      do j=1,ndev
c
c      Plot individual device characteristics
c
         write(12,107)
         title='Characteristics for device '//trim(dev(j))
         call add_toc_item(3,title)
c
         title=' Percentage time active for device '//dev(j)
         do i=1,nline
            x(i)=tmact(i,j)
         enddo
         if(nq.gt.1)call summer(x,nsamples,1)
         call plotutil(x,t,period0,nsamples)
c
         title=' KiloBytes per second for device '//dev(j)
         do i=1,nline
            y(i)=akbps(i,j)
            ydata(i)=y(i)
         enddo
         if(nq.gt.1)call summer(y,nsamples,1)
         call plotutil(y,t,period0,nsamples)
c
         title=' KiloBytes per second written to device '//dev(j)
         do i=1,nline
            y(i)=rintsecs*float(kbwrtn(i,j))
            ydata(i)=y(i)
         enddo
         if(nq.gt.1)call summer(y,nsamples,1)
         call plotutil(y,t,period0,nsamples)
c
         title=' KiloBytes per second read from device '//dev(j)
         do i=1,nline
            y(i)=rintsecs*float(kbread(i,j))
            ydata(i)=y(i)
         enddo
         if(nq.gt.1)call summer(y,nsamples,1)
         call plotutil(y,t,period0,nsamples)
c
         title=' Kilobytes/transaction for device '//dev(j)
         do i=1,nline
            if(tps(i,j).gt.0)then
                                    x(i)=akbps(i,j)/tps(i,j)
                                else
                                    x(i)=0.
            endif
         enddo
         if(nq.gt.1)call summer(x,nsamples,1)
         call plotutil(x,t,period0,nsamples)
c
         title=' KiloBytes/S/% tm_act for device '//dev(j)
         do i=1,nline
            xdata(i)=tmact(i,j)
            if(tmact(i,j).gt.0.)then
                                    x(i)=akbps(i,j)/tmact(i,j)
                                else
                                    x(i)=0.
            endif
         enddo
c
         if(nq.gt.1)call summer(x,nsamples,1)
         call plotutil(x,t,period0,nsamples)
c
         title=' tps for device '//dev(j)
         do i=1,nline
            x(i)=tps(i,j)
         enddo
         if(nq.gt.1)call summer(x,nsamples,1)
         call plotutil(x,t,period0,nsamples)
c
         title=' KiloBytes read from device '//dev(j)
         do i=1,nline
            x(i)=kbread(i,j)
         enddo
         if(nq.gt.1)call summer(x,nsamples,0)
         call plotutil(x,t,period0,nsamples)
c
         title=' KiloBytes written to device '//dev(j)
         do i=1,nline
            x(i)=kbwrtn(i,j)
         enddo
         if(nq.gt.1)call summer(x,nsamples,0)
         call plotutil(x,t,period0,nsamples)
c
      enddo
c
      call close_lists(1)
      write(10,102)
c
100   format(a100)
101   format(a85)
102   format('<HR>')
200   format(' ***** CPU USAGE WARNING in line',i6,':'
     *      ,'CPU=',f8.3,', USR=',f8.3,', SYS=',f8.3)
202   format('PERIOD=',e9.2,'? ',a15
     *      ,' %act:',f6.1
     *      ,' Tot KB/S:',f9.1
     *      ,' tps:'    ,f6.1
     *      ,' KB_read:',f9.1
     *      ,' KB_wrtn:',f9.1
     *      ,' Record:',i5)
103   format(2i6,6f11.1)
1031  format( i6,3f11.1,2i11)
104   format(1x,a15,85a1)
105   format(a12)
106   format(/'Number of records in error:',i9
     *      ,/'        Number of readings:',i9
     *      ,/' Probable sample times: Max=',e12.5,', Mean=',e12.5
     *      , ', Min=',e12.5,/)
107   format(/)
108   format(' Error in CPU read in section',i5,/1x,a15,85a1)
109   format(' Error in dev read in section',i5,/1x,a15,85a1)
110   format('<BODY> <A NAME=Top_Of_Page></A>'
     *     ,/'<H1>'
     *     ,/'SYSTEM PERFORMANCE ANALYSIS <br><br>'
     *     ,/'IBM AIX IOSTAT <br><br>'
     *     ,/a100,'<br><br>'
     *     ,/a50 ,'<br><br>'
     *     ,/'Design and copyright reserved J S Watts 2012<br><br>'
     *     ,/'</H1>'
     *     ,/'<HR><H2><A NAME=ToC>Table of Contents</A></H2>'
     *      )
111   format(/,' Aspects of workload - summary chart             '
     *      ,'     Max    Highest mean     Min    Period Avg T'
     *      ,'     Average Standrd dev',/)
112   format(/,36x,'Samples        '       
     *      ,'Mean Standrd dev       Slope Y-intercept X-intercept'
     *      ,'       r    r**2')
113   format(' System_name: ',a100
     *     ,/'     Version: ',a50)
115   format(8a1,3i3,f8.2)
116   format(/'** WARNING - LIMIT ON LINES OF DATA REACHED **',//)
1161  format(/'** WARNING - TOO MANY DISKS FOUND - LIMIT IS',i4,'**',/)
1162  format(/'** WARNING - ERROR IN CPU LINE',i8,6f12.2,'**')
1163  format(/'                      CPU LINE',i8,6f12.2,'**')
217   format(a10,' - no activity recorded on this device')
119   format(
     *       /'.fo left',/
     *      ,/'If this system displays high-levels of I/O wait,'
     *      ,/'(see chart for CPU - I/O wait state), then'
     *      ,/'use the hdisk table above.'
     *     ,//'The disks with positive values of r, and values of'
     *      ,/'r**2 closest to 1.0, are the ones likely'
     *      ,/'to be causing the bottlenecks. Find out what'
     *      ,/'they are being used for, and check contention'
     *      ,/'or split their workloads.' 
     *      ,/'.fo off'
     *      ,/
     *      )
120   format(' Data records start at ',a15,85a1)
121   format(//' Attribute: ',a30,//)
122   format('***** ERROR at line',i10) 
123   format('File complete.  Lines read so far=',i9)
124   format('          NDEVMAX increased from',i5,' to',i5)
125   format(/'***** Last NDEV was',i3,' - NDEVMAX is',i3,/)
126   format(/'##### ERROR in IOSTAT.F - NO RECORDS READ #####',/)
        stop
        end
