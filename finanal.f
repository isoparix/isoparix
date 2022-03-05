      program finanal
c
c      Analyses Bank, GoldCard and Amex statements
c
      implicit real(8) (a-h,o-z)      
c
      real(8) income
c
      logical euro
c
      character(180) line_text,description
      character(1) amtext(20),line_textb(180),datext(14),ig(28)
      character(14)date,minnatwdate,minamexdate,maxnatwdate,maxamexdate
     *            ,isotext
      character(18)baldate(-100:4096)
      character(20)amtexta
      character(30)graph_name,series_name
      character(28)isodates
      character(3)category,month(12),montha
c
      dimension idquote(20),idcomma(20),ndays(12)
     *          ,balhi(-100:4096),ballo(-100:4096),income(-100:4096)
     *          ,points_array_hi(4096)
     *          ,points_array_lo(4096)
     *          ,points_array_in(4096)
     *          ,fracmonx(4096)
c
      equivalence(amtexta,amtext),(line_text,line_textb),(date,datext)
     *          ,(ig,isodates)
c      
      minnatwday=100000
      minamexday=100000
      maxnatwday=0
      maxamexday=0
c      
      ndays( 1)=  0 ! 31
      ndays( 2)= 31 ! 29
      ndays( 3)= 60 ! 31
      ndays( 4)= 91 ! 30
      ndays( 5)=121 ! 31
      ndays( 6)=152 ! 30
      ndays( 7)=182 ! 31
      ndays( 8)=213 ! 31
      ndays( 9)=244 ! 30
      ndays(10)=274 ! 31
      ndays(11)=305 ! 30
      ndays(12)=335 ! 31
c      
      month( 1)='Jan'
      month( 2)='Feb'
      month( 3)='Mar'
      month( 4)='Apr'
      month( 5)='May'
      month( 6)='Jun'
      month( 7)='Jul'
      month( 8)='Aug'
      month( 9)='Sep'
      month(10)='Oct'
      month(11)='Nov'
      month(12)='Dec'
c      
      income=0.
      balhi=-1000000.
      ballo= 1000000.
      balmax=-1000000.
      balmin= 1000000.
      maxmon=-1000000
      minmon=1000000
c      
      nline=0
  1   continue
      read(*,100,end=2,err=2)line_text
      nline=nline+1
      if(line_textb(12).eq.','.and.
     *   line_textb(13).eq.','
     *  )then
             go to 1
      endif
c
      read(line_text,107)date
      if(datext(3).eq.'/'
     *  )then
             datext(3)=' '
             datext(6)=' '
             datext(11)=' '
             read(date,105,err=4)iday,imon,iyear
c        
             if(mod(iyear,4).eq.0
     *         )then
                    leap_year=1
                else
                    leap_year=0
             endif
c        
             montha=month(imon)
             read(line_text,108)category
         else
             imon=-1   ! Error check
             read(date,1051,err=4)montha
             if(montha.eq.'Jan')imon= 1
             if(montha.eq.'Feb')imon= 2
             if(montha.eq.'Mar')imon= 3
             if(montha.eq.'Apr')imon= 4
             if(montha.eq.'May')imon= 5
             if(montha.eq.'Jun')imon= 6
             if(montha.eq.'Jul')imon= 7
             if(montha.eq.'Aug')imon= 8
             if(montha.eq.'Sep')imon= 9
             if(montha.eq.'Oct')imon=10
             if(montha.eq.'Nov')imon=11
             if(montha.eq.'Dec')imon=12
             if(imon.lt.0
     *         )then
                    go to 1
                else
                    read(date,1052,err=4)iday,iyear
             endif
             read(line_text,109)category
      endif
      write(date,106)iday,imon,montha,iyear
      lday=ndays(imon)+iday
c        
      if(imon.gt.2
     *  )then
             lday=lday+leap_year
      endif
c        
c        lday is now 'day of the year'
c        
      mday=((iyear-2000)*365)+lday
c        
      if(category.eq.'"Re'
     *  )then
c        
             if(mday.lt.minamexday
     *         )then
                    minamexday=mday
                    minamexdate=date
             endif
c        
             if(mday.gt.maxamexday
     *         )then
                    maxamexday=mday
                    maxamexdate=date
             endif
c        
         else
c        
             if(mday.lt.minnatwday
     *         )then
                    minnatwday=mday
                    minnatwdate=date
c                   write(0,*)minnatwday,minnatwdate
             endif
c        
             if(mday.gt.maxnatwday
     *         )then
                    maxnatwday=mday
                    maxnatwdate=date
c                   write(0,*)maxnatwday,maxnatwdate
             endif
c        
      endif
c        
      mmon=((iyear-2000)*12)+imon
      fracday=float(iyear+lday)/float(365+leap_year)
      fracmon=float(iyear)+(float(imon-1)/12.)
      fracmonx(mmon)=fracmon
      if(mmon.gt.maxmon
     *  )then
             maxmon=mmon
             write(2,124)maxmon,fracmonx(maxmon)
      endif
      if(mmon.lt.minmon
     *  )then
             minmon=mmon
             write(2,123)minmon,fracmonx(minmon)
      endif
c      
      if(category.eq.'   '.or.
     *   category.eq.'Des'.or.
     *   category.eq.'STA'.or.
     *   category.eq.'PAY'
     *  )then
             go to 1
      endif
c     write(*,110)date,category
c      
      nquotes=0
      ncommas=0
c      
      euro=.false.
c      
      do i=1,176
c      
         if(line_textb(i  ).eq.' '.and.
     *      line_textb(i+1).eq.'E'.and.
     *      line_textb(i+2).eq.'U'.and.
     *      line_textb(i+3).eq.'R'.and.
     *     (line_textb(i+4).eq.' '.or.
     *      line_textb(i+4).eq.'"')
     *     )then
                euro=.true.
      endif
c      
         if(line_textb(i).eq.'"'
     *     )then
                nquotes=nquotes+1
                idquote(nquotes)=i
         endif
c      
         if(line_textb(i).eq.','
     *     )then
                ncommas=ncommas+1
                idcomma(ncommas)=i
         endif
c      
      enddo
c      
      if(category.eq.'"Re'
     *  )then
             icasha=idquote(3)+1
             icashb=idquote(4)-1
c
             n=1
             do i=icasha,icashb
                amtext(n)=line_textb(i)
                n=n+1
             enddo
             do i=n,20
                amtext(i)=' ' 
             enddo
             read(amtexta,*,err=3)value
             if(value.lt.0.0)go to 1         ! AmEx payment
             namea =idquote(5)+1
             nameb =idquote(6)-1
             description=line_text(namea:nameb)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
         else 
             icasha=idquote(2)+2
             icashb=idquote(3)-2
             amtext=''
             n=1
             do i=icasha,icashb
                amtext(n)=line_textb(i)
                n=n+1
             enddo
             if(line_text(idquote(nquotes-1)+2:idquote(nquotes)-1).eq.
     *          '606040-53423038'
     *         )then
                    read(amtexta,*,err=3)value,balance
                    value=-value
c
                    if(balance.gt.balhi(mmon)
     *                )then
                           balhi(mmon)=balance
                           if(balance.gt.balmax)balmax=balance
                           write(baldate(mmon),112)montha,iyear,fracmon
                    endif
c
                    if(balance.lt.ballo(mmon)
     *                )then
                           ballo(mmon)=balance
                           if(balance.lt.balmin)balmin=balance
                           write(baldate(mmon),112)montha,iyear,fracmon
                    endif
c
                    write(4,103)value,balance,mday,date
                else
                    read(amtexta,*,err=3)value
             endif
      endif
c
      if(category.eq.'BAC'
     *  )then                      ! This is income...
             income(mmon)=income(mmon)-value
             description=line_text(idcomma(2)+3:idcomma(3)-2)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'D/D'.or.
     *   category.eq.'SAL'.or.
     *   category.eq.'IBP'.or.
     *   category.eq.'DEB'.or.
     *   category.eq.'SBT'.or.
     *   category.eq.'TMS'.or.
     *   category.eq.'CRE'.or.
     *   category.eq.'BGC'
     *  )then
             description=line_text(idcomma(2)+3:idcomma(3)-2)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'POS'.or.
     *   category.eq.'TSU'.or.
     *   category.eq.'DPC'
     *  )then
             description=line_text(idcomma(3)+2:idcomma(4)-2)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'STF'.or.
     *   category.eq.'CHP'.or.
     *   category.eq.'UTF'
     *  )then
             description=line_text(idcomma(4)+2:idcomma(5)-2)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'S/O'.or.
     *   category.eq.'ITL'
     *  )then
             description=line_text(idcomma(2)+3:idcomma(3)-2)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'C/L'
     *  )then
             description='CASH WITHDRAWAL'
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'CHQ'
     *  )then
             description='CHEQUES'
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'INT'
     *  )then
             description='INTEREST'
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      if(category.eq.'TFR'
     *  )then
             description=line_text(idcomma(6)+2:idcomma(7)-2)
             write(*,1011)date,category,value,trim(description)
             if(euro
     *         )then
                    write(*,1012)date,category,value,trim(description)
                else
                    write(*,1011)date,category,value,trim(description)
             endif
              go to 1
      endif
c
      write(40,101)date,category
c      
      go to 1
c      
  2   continue
c
      isodates=minamexdate//maxamexdate
c     write(0,*)isodates
      isotext=ig( 1)//ig( 2)//ig( 7)//ig( 8)//ig( 9)//ig(13)//ig(14)
     *      //ig(15)//ig(16)//ig(21)//ig(22)//ig(23)//ig(27)//ig(28)
      write(0,120)minamexdate,maxamexdate,isotext
c
      isodates=minnatwdate//maxnatwdate
c     write(0,*)isodates
      isotext=ig( 1)//ig( 2)//ig( 7)//ig( 8)//ig( 9)//ig(13)//ig(14)
     *      //ig(15)//ig(16)//ig(21)//ig(22)//ig(23)//ig(27)//ig(28)
      write(0,121)minnatwdate,maxnatwdate,isotext
c      
      xmin=fracmonx(minmon)
      xmax=fracmonx(maxmon)
      write(2,103)xmin,xmax
      graph_name='finrecord'
      call grf_header(xmin,xmax,balmin,balmax,graph_name)
      np=1
      do n=minmon,maxmon
         write(2,111)minmon,n,maxmon,fracmonx(n)
     *              ,baldate(n),ballo(n),balhi(n)
         points_array_hi(np)=fracmonx(n)
         points_array_lo(np)=fracmonx(n)
         points_array_in(np)=fracmonx(n)
         np=np+1
         points_array_hi(np)=balhi(n)
         points_array_lo(np)=ballo(n)
         points_array_in(np)=income(n)
         np=np+1
      enddo
      np=np-1
      series_name='High balance'
      call grf_points(0,1,np,points_array_hi,'Blue    ','Blue    '
     *               ,0,1,series_name)
      series_name='Low balance'
      call grf_points(0,2,np,points_array_lo,'Red     ','Red     '
     *               ,0,1,series_name)
      series_name='Income'
      call grf_points(0,3,np,points_array_in,'Green   ','Green   '
     *               ,0,1,series_name)
      call grf_trailer(3)
      stop      
c      
  3   continue
      write(*,104)amtexta,date
      stop
c      
  4   continue
      write(*,104)date,nline
      stop
c      
100   format(a180)
101   format(a14,a5,f12.2,2x,180a1)
1011  format(a14,a4,f12.2,2x,a)
1012  format(a14,a4,f12.2,2x,'Euro:   ',a)
102   format(49x,10a1)
103   format(2f10.2,i8,2x,a)
104   format('ERROR: DATE is ',a20,' at line'i3)
105   format(i2,i3,i5)
1051  format(3x,a3)
1052  format(i2,5x,i4)
106   format(i2.2,i3.2,a4,i5)
107   format(a14)
108   format(11x,a3)
109   format(12x,a3)
110   format(a14,a4)
111   format(3i6,f10.3,a20,2f11.2)
112   format(a3,i5,f10.3)
120   format('AmEx    dates are ',2a20,5x
     *      ,'      c:\data\finance_jsw\AMEX\downloads\AMEX',a14,'.csv')
121   format('NatWest dates are ',2a20,5x
     *      ,'c:\data\finance_jsw\NATWEST\downloads\NATWEST',a14,'.csv')
122   format(2(a2,4x,a3,3x,a2))
123   format('MINMON:',i6,f10.2)
124   format('MAXMON:',i6,f10.2)
c
      end      
