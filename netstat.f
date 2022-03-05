        program netstanal
      use allcomms
c
c        Analyses unix netstat data
c
        character (1)  dataline(99),devdet(9)
        character (9)  devtype
        character (99) datbline
c
        equivalence (datbline,dataline),(devdet,devtype)
c
        logical firstdata,section
c
c       version=' NETSTANAL Version 1.0  3 March 2000'
c       version=' NETSTANAL Version 1.1  24 March 2000'
c       version=' NETSTANAL Version 1.2  26 March 2000'
c       version=' NETSTANAL Version 1.21 16 January 2001'
        version=' NETSTANAL Version 1.22 8th July 2003'
c
c####### #### Read the netstat period, and the averaging quantity ###
c
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
      call mem_alloc('netstat ')
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
        read(*,100,end=99,iostat=irc,err=98)(dataline(mx),mx=1,99)
        nerrors=0
c
c      Look for an EOF line..
c
        devtype=dataline(1)//dataline(2)//dataline(3)
     *        //dataline(4)//dataline(5)//dataline(6) 
     *        //dataline(7)//dataline(8)//dataline(9) 
        if(devtype.eq.'EndofFile')then
                                       write(*,123)nline
                                       go to 2
        endif
c
      if(devtype.eq.'    input'
     *  )then
c            write(*,*)devtype
c
c      Read next two lines as junk, and then one as data
c
             do i=1,3
        read(*,100,end=99,iostat=irc,err=98)(dataline(mx),mx=1,99)
             enddo
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
c     write(*,*)datbline
         read(datbline,*,iostat=irc,err=98)
     *                    priminpak(nline),priminerr(nline)
     *                   ,primexpak(nline),primexerr(nline)
     *                   ,primcoll (nline)
     *                   , totinpak(nline), totinerr(nline)
     *                   , totexpak(nline), totexerr(nline)
     *                   , totcoll (nline)
c       write(       *,805)nline
c    *                   ,priminpak(nline),priminerr(nline)
c    *                   ,primexpak(nline),primexerr(nline)
c    *                   ,primcoll (nline)
c    *                   , totinpak(nline), totinerr(nline)
c    *                   , totexpak(nline), totexerr(nline)
c    *                   , totcoll (nline)
805   format(i6,10f6.1)
c
c      Deal with an incomplete record
c
      if(irc.ne.0)then
                      write(*,*)datbline
                      nline=nline-1
                      go to 4
      endif
c
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
c        All data read for this instance of netstat
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
      xname='All adapters - input packets                   '
      yname='All adapters - output packets                   '
      call lsquare(totinpak,totexpak,nline)
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
c   
      title='Primary adapter - input packets                   '
      if(nq.gt.1)call summer(priminpak,nsamples,1)
      call plotutil(priminpak,t,period0,nsamples)
c   
      title='Primary adapter - input errors                   '
      if(nq.gt.1)call summer(priminerr,nsamples,1)
      call plotutil(priminerr,t,period0,nsamples)
c   
      title='Primary adapter - output packets                   '
      if(nq.gt.1)call summer(primexpak,nsamples,1)
      call plotutil(primexpak,t,period0,nsamples)
c   
      title='Primary adapter - output errors                   '
      if(nq.gt.1)call summer(primexerr,nsamples,1)
      call plotutil(primexerr,t,period0,nsamples)
c   
      title='Primary adapter - collisions                     '
      if(nq.gt.1)call summer(primcoll,nsamples,1)
      call plotutil(primcoll,t,period0,nsamples)
c   
      title='All adapters - input packets                   '
      if(nq.gt.1)call summer(totinpak,nsamples,1)
      call plotutil(totinpak,t,period0,nsamples)
c   
      title='All adapters - input errors                   '
      if(nq.gt.1)call summer(totinerr,nsamples,1)
      call plotutil(totinerr,t,period0,nsamples)
c   
      title='All adapters - output packets                   '
      if(nq.gt.1)call summer(totexpak,nsamples,1)
      call plotutil(totexpak,t,period0,nsamples)
c   
      title='All adapters - output errors                   '
      if(nq.gt.1)call summer(totexerr,nsamples,1)
      call plotutil(totexerr,t,period0,nsamples)
c   
      title='All adapters - collisions                     '
      if(nq.gt.1)call summer(totcoll,nsamples,1)
      call plotutil(totcoll,t,period0,nsamples)
c
      title='All adapters - total packets transferred      '
      do n=1,nline
         x(n)=totinpak(n)+totexpak(n)
      enddo
      if(nq.gt.1)call summer(x,nsamples,1)
      call plotutil(x,t,period0,nsamples)
c
      call close_lists(1)
      write(10,102)
c
100        format(99a1)
102        format('<HR>')
104        format(1x,a9,99a1)
105        format(a12)
106        format(' Number of readings=',i9,/)
107        format(/)
108        format(' Error in CPU read in section',i5,/1x,a9,99a1)
109        format(' Error in dev read in section',i5,/1x,a9,99a1)
110   format('<BODY> <A NAME=Top_Of_Page></A>'
     *     ,/'<H1>'
     *     ,/'SYSTEM PERFORMANCE ANALYSIS <br><br>'
     *     ,/'IBM AIX NETSTAT <br><br>'
     *     ,/a100,'<br><br>'
     *     ,/a50 ,'<br><br>'
     *     ,/'Design and copyright reserved J S Watts 2012<br><br>'
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
120   format(' Data records start at ',a9,99a1)
121   format(//' Attribute: ',a30,//) 
122   format('***** ERROR at line',i10)
123   format('File complete.  Lines read so far=',i9)
        stop
        end
