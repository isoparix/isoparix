      program psa_anal
c
c      Reads day, month, year, PSA level - converts to CSV with
c      decimal year and PSA level
c      
      dimension ndays(12)
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
c
   1  continue
      write(*,101)
      read(*,*,end=2)iday,imon,iyear,psa
c        
      if(mod(iyear,4).eq.0
     *  )then
             leap_year=1
         else
             leap_year=0
      endif
c     
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
      day=float(mday)/365.25
c
      write(*,100)day,psa
      go to 1
  2   continue
      stop
100   format(f10.6,',',f0.2)
101   format('Enter integer day, month, year and decimal PSA level')
      end
