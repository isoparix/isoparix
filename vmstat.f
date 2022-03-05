        program vmstanal
c
        use allcomms
c
        character (1)  dataline(99),devdet(9)
        character (9)  devtype
        character (99) datbline
c
        equivalence (datbline,dataline)
        equivalence (datbline,devtype )
        equivalence (devdet,devtype)
c
        logical firstdata,section
c
c       version=' VMSTANAL Version 1.0  2 December 1997'
c       version=' VMSTANAL Version 1.1 27 January  1998'
c       version=' VMSTANAL Version 1.1 28 January  1998'
c       version=' VMSTANAL Version 3.2 30 September 1998'
c       version=' VMSTANAL Version 3.3 22 November 1998'
c       version=' VMSTANAL Version 3.4 25 November 1998'
c       version=' VMSTANAL Version 3.5  1 December 1998'
c       version=' VMSTANAL Version 3.6 18 January 1999'
c       version=' VMSTANAL Version 3.7 24 March 2000'
c       version=' VMSTANAL Version 3.8 26 March 2000'
c       version=' VMSTANAL Version 3.81 16 January 2001'
c       version=' VMSTANAL Version 3.9  25th June 2001'
c       Version=' VMSTANAL Version 3.91 22nd August 2001'
c       Version=' VMSTANAL Version 3.92 8th July 2003'
c
c############ Read the vmstat period, and the averaging quantity ###
c
      read(*,*)nqsamp
c
      maxdevs     =0
      maxdev_s    =0
      maxhosts    =0
      maxtimes    =0
      maxrows     =0
      max_screen_x=0
      max_screen_y=0
c
      call mem_alloc('vmstat  ')
c
        read (*,*)sysold,intsecs,nq,it
c
c      Open all the output files
c
        call open_files
c
        write(10,110)sysold,version
        write( 8,113)sysold,version
        write(12,113)sysold,version
        write(14,113)sysold,version
c
        lsqcount=0
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
        maxta=0
        nline=0
        nerrors=0
  2     continue
        firstdata=.true.
        ta=tainit
c
c      Make sure that data lines start on an averaging boundary
c
        nline=(nline/nq)*nq
c                                                                   
   4    continue
        read(*,100,end=99,iostat=irc,err=98)datbline
        nerrors=0
c
c      Look for an EOF line..
c
        if(devtype.eq.'EndofFile')then
                                       write(*,123)nline
                                       go to 2
        endif
c
        if(
     *     (dataline(2).ne.'0').and.
     *     (dataline(2).ne.'1').and.
     *     (dataline(2).ne.'2').and.
     *     (dataline(2).ne.'3').and.
     *     (dataline(2).ne.'4').and.
     *     (dataline(2).ne.'5').and.
     *     (dataline(2).ne.'6').and.
     *     (dataline(2).ne.'7').and.
     *     (dataline(2).ne.'8').and.
     *     (dataline(2).ne.'9')
     *    )then
               go to 4
        endif
c                                         
        if(firstdata
     *    )then
               firstdata=.false.
               go to 4
        endif
c
        if(nline.ge.nqsamp)then           
                             write(*,116) 
                             go to 99     
        endif                             
c                                         
        nline=nline+1
        t(nline)=ta
        period0(nline)=float(intsecs) 
c
c      Read in the data from the text buffer...
c
         read(datbline,*,iostat=irc,err=98)
     *                     r(nline),  b(nline),avm(nline),fre(nline)
     *                   ,re(nline), pi(nline), po(nline), fr(nline)
     *                   ,sr(nline), cy(nline), in(nline),syf(nline)
     *                   ,cs(nline)
     *        ,cpuser(nline),cpsys(nline),cpidle(nline),cpiow(nline)
c     if(b(nline).gt.30)write(*,*)datbline
c
c      Deal with an incomplete record
c
      if(irc.ne.0)then
                      write(*,*)datbline
                      nline=nline-1
                      go to 4
      endif
 
c       write(30,805)      r(nline),  b(nline),avm(nline),fre(nline)
c    *                   ,re(nline), pi(nline), po(nline), fr(nline)
c    *                   ,sr(nline), cy(nline), in(nline),syf(nline)
c    *                   ,cs(nline)
c    *        ,cpuser(nline),cpsys(nline),cpidle(nline),cpiow(nline)
805   format(2f5.1,15f8.1)
        ta=ta+float(intsecs)
        ta=mod(ta,.864E+05)
        if(int(ta).gt.maxta)maxta=ta  
        go to 4
c
c      Handle errors....
c
 98   continue
      write(*,122)nline
      firstdata=.true.
      nline=nline-1
c
c      If no data lines have been read between this error and
c      the last, then give up.  Otherwise, read more data,
c      probably from the next file
c
      nerrors=nerrors+1
      if(nerrors.eq.1)go to 2
c
c*************************************
c
c        All data read for this instance of vmstat
c
 99   continue
c
c        All data read
c
        close(3)
        write(*,106)nline
c
c ********* TEST ***********
c     stop
c ********* TEST ***********
c
c      Do some correlations...
c
      write(14,112)
c
c     do i=1,nline
c        if(pi(i).gt.0..or.po(i).gt.0.)then
c                                     write(60,700)
c    *               avm(i),pi(i),po(i),pi(i)+po(i)
c        endif
c     enddo
700   format(4f10.1)
c
      xname='CPU (system+user)            '      
      yname='CPU (wait+idle)  !!CHECK!!   '      
      do i=1,nline
         x(i)=cpuser(i)+cpsys(i)
         y(i)=cpiow(i)+cpidle(i)
      enddo
      call lsquare(x,y,nline)
c
      xname='CPU (system+user)            '      
      yname='CPU wait                     '      
      call lsquare(x,cpiow,nline)
c
      xname='Number threads in run queue  '      
      yname='CPU (system+user)            '      
      call lsquare(r,x,nline)
c
      xname='Number of blocked threads    '
      yname='CPU wait                     '      
      call lsquare(b,cpiow,nline)
c
      xname='Total CPU                  =='      
      yname='Active virtual memory (avm)=='
      call lsquare(x,avm,-nline)
      write(8,121)yname
      call predict(muy,sdy,1,nline)
c
      xname='Free memory                  '      
      yname='Non-zero page-ins per second '      
      call nzy(fre,pi,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero page-outs per second'      
      call nzy(fre,po,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero pi+po per second    '      
      do i=1,nline
         y(i)=pi(i)+po(i)
         if(fr(i).gt.0.)then
                             x(i)=sr(i)/fr(i)
                         else
                             x(i)=0.
        endif
      enddo 
      call nzy(fre,y,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      xname='Free memory'
      yname='Ratio_pages_scanned:freed'
      call lsquare(fre,x,nline)
c
      xname='Non-zero page-outs per second'      
      yname='Non-zero page-ins  per second '      
      call nzxy(po,pi,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      xname='Active virtual memory (avm)  '      
      yname='Non-zero page-ins per second '      
      call nzy(avm,pi,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero page-outs per second'      
      call nzy(avm,po,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero pi+po per second    '      
      do i=1,nline
         y(i)=pi(i)+po(i)
      enddo 
      call nzy(avm,y,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero pages freed'
      call nzy(avm,fr,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
c      Now use minimum avm values...
c
      xname='Minimum avm values           '      
      yname='Non-zero page-ins per second '      
      call memsmooth(avm,pi,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero page-outs per second'      
      call memsmooth(avm,po,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero pi+po per second    '      
      call memsmooth(avm,y,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
      yname='Non-zero pages freed'
      call memsmooth(avm,fr,nsamples)
      call lsquare(xdata,ydata,nsamples)
c
c      *********** END OF CORRELATIONS - NO AVERAGES **************
c
c      Start the daily charts
c
c
                     call summer(period0,nsamples,0)
c
c      Plot overall system characteristics
c
         write(12,111)
         call add_toc_item(2,'Overall system characteristics')
c   
      title='Total number of kernel threads'
      do n=1,nline
         x(n)=r(n)+b(n)
      enddo
      if(nq.gt.1)call summer(x   ,nsamples,1)
      call plotutil(x   ,t,period0,nsamples)
c   
      title='r/(r+b) - fraction of threads in run queue'
      do n=1,nline
         x(n)=r(n)/(r(n)+b(n))
      enddo
      if(nq.gt.1)call summer(x   ,nsamples,1)
      call plotutil(x   ,t,period0,nsamples)
c   
      title='r   - Number of kernel threads placed in run queue'
      if(nq.gt.1)call summer(r   ,nsamples,1)
      call plotutil(r   ,t,period0,nsamples)
c
      title='b   - Number of kernel threads placed in wait queue'
      if(nq.gt.1)call summer(b   ,nsamples,1)
      call plotutil(b   ,t,period0,nsamples)
c
      title='avm - Active virtual pages'
      if(nq.gt.1)call summer(avm ,nsamples,1)
      call plotutil(avm ,t,period0,nsamples)
c
      title='fre - Size of the free list'
      if(nq.gt.1)call summer(fre ,nsamples,1)
      call plotutil(fre ,t,period0,nsamples)
c
      title='re  - Pager input/output list'
      if(nq.gt.1)call summer(re  ,nsamples,1)
      call plotutil(re  ,t,period0,nsamples)
c
      title='pi  - Pages paged in from paging space'
      if(nq.gt.1)call summer(pi  ,nsamples,1)
      call plotutil(pi  ,t,period0,nsamples)
c
      title='po  - Pages paged out to paging space'
      if(nq.gt.1)call summer(po  ,nsamples,1)
      call plotutil(po  ,t,period0,nsamples)
c
      title='fr  - Pages freed (page replacement)'
      if(nq.gt.1)call summer(fr  ,nsamples,1)
      call plotutil(fr  ,t,period0,nsamples)
c
      title='sr  - Pages scanned by page-replacement algorithm'
      if(nq.gt.1)call summer(sr  ,nsamples,1)
      call plotutil(sr  ,t,period0,nsamples)
c
      title='(Pages_scanned)/(Pages_freed) ratio'
      do mx=1,nline
         if(fr(mx).gt.0.)then
                             x(mx)=sr(mx)/fr(mx)
                         else
                             x(mx)=0.
         endif
      enddo
      if(nq.gt.1)call summer(x  ,nsamples,1)
      call plotutil(x  ,t,period0,nsamples)
c
      title='cy  - Clock cycles by page-replacement algorithm'
      if(nq.gt.1)call summer(cy  ,nsamples,1)
      call plotutil(cy  ,t,period0,nsamples)
c
      title='in  - Device interrupts'
      if(nq.gt.1)call summer(in  ,nsamples,1)
      call plotutil(in  ,t,period0,nsamples)
c
      title='sycalls  - System calls'
      if(nq.gt.1)call summer(syf,nsamples,1)
      call plotutil(syf,t,period0,nsamples)
c
      title='cs  - Kernel thread context switches'
      if(nq.gt.1)call summer(cs ,nsamples,1)
      call plotutil(cs ,t,period0,nsamples)
c
      title='us  - User time'
      if(nq.gt.1)call summer(cpuser ,nsamples,1)
      call plotutil(cpuser ,t,period0,nsamples)
c
      title='sy  - System time'
      if(nq.gt.1)call summer(cpsys,nsamples,1)
      call plotutil(cpsys,t,period0,nsamples)
c
      title='id  - CPU idle time'
      if(nq.gt.1)call summer(cpidle ,nsamples,1)
      call plotutil(cpidle ,t,period0,nsamples)
c
      title='wa  - Process is wait, with pending disk I/O'
      if(nq.gt.1)call summer(cpiow ,nsamples,1)
      call plotutil(cpiow ,t,period0,nsamples)
c
      call close_lists(1)
      write(*,102)
c
        stop
c
100   format(a99)
102   format('<HR>')     
105   format(a12)
106   format(' Number of readings=',i9,/)
107   format(/)
108   format(' Error in CPU read in section',i5,/1x,a9,99a1)
109   format(' Error in dev read in section',i5,/1x,a9,99a1)
110   format('<BODY> <A NAME=Top_Of_Page></A>'
     *     ,/'<H1>'
     *     ,/'SYSTEM PERFORMANCE ANALYSIS <br><br>'
     *     ,/'IBM AIX VMSTAT <br><br>'
     *     ,/a100,'<br><br>'
     *     ,/a50 ,'<br><br>'
     *     ,/'Design and copyright reserved J S Watts 2011 <br><br>'
     *     ,/'</H1>'
     *     ,/'<HR><H2><A NAME=ToC>Table of Contents</A></H2>'
     *      )
111   format(/,' Aspects of workload - summary chart             '
     *      ,'     Max    Highest mean     Min    Period Avg T'
     *      ,'     Average Standrd dev     p',/)
112   format(/,36x,'Samples        '
     *      ,'Mean Standrd dev       Slope Y-intercept X-intercept'
     *      ,'      r   r**2')
113   format(' System_name: ',a100
     *     ,/'     Version: ',a50) 
115   format(8a1,3i3,f8.2)
116   format(
     *       /'*******************************************'  
     *      ,/'*******************************************'  
     *      ,/'*** WARNING - END OF DATA SPACE REACHED ***'
     *      ,/'*******************************************'  
     *      ,/'*******************************************'  
     *      ,//)
217   format(a10,' - no activity recorded on this device')
121   format(//' Attribute: ',a30,//) 
122   format('***** ERROR at line',i10)
123   format('File complete.  Lines read so far=',i9)
c
        end
