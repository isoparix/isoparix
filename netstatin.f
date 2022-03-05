        program netstanal
c
c        Analyses unix netstat data
c
      real*4 priminpak,priminerr,primexpak,primexerr,primcol
     *      , totinpak, totinerr, totexpak, totexerr, totcol
c
        character *12 filename
        character *1  dataline(99),devdet(9)
        character *9  devtype
        character *99 datbline
c
        equivalence (datbline,dataline),(devdet,devtype)
     *             ,(devtype ,dataline)
c
        logical firstdata,section
c
c       version=' NETSTATIN Version 1.0  27 April 2003'
c
c                                                                   
   4    continue
        read(*,100,end=99,iostat=irc,err=98)(dataline(mx),mx=1,99)
c
      if(devtype.eq.'    input'
     *  )then
             write(8,103)0,0,0,0,0,0,0,0,0,0
c
c      Read next two lines as junk, and then one as data
c
             do i=1,3
        read(*,100,end=99,iostat=irc,err=98)(dataline(mx),mx=1,99)
             enddo
      endif
c                                         
c      Read in the data from the text buffer...
c
         read(datbline,*,iostat=irc,err=98)
     *                    priminpak,priminerr
     *                   ,primexpak,primexerr
     *                   ,primcoll 
     *                   , totinpak, totinerr
     *                   , totexpak, totexerr
     *                   , totcoll 
        write(      8 ,103)priminpak,priminerr
     *                   ,primexpak,primexerr
     *                   ,primcoll 
     *                   , totinpak, totinerr
     *                   , totexpak, totexerr
     *                   , totcoll 
c
      go to 4
c
 98   continue
c
c      Deal with an incomplete record
c
      if(irc.ne.0)then
c                     write(*,101)
                      go to 4
      endif
c
 99   continue
      stop
c
100   format(99a1)
101   format('Incomplete data record')
103   format(10(f10.1,','))
104   format(1x,a9,99a1)
105   format(a12)
        end
