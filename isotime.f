      subroutine isotime(iday,amon,nyr,ihrs,mins,isecs,itod,ityear,lday)
c
c      Takes in 23 Jun 12 22 11 00 and 
c      returns time of day and time of year
c
      implicit real(8) (a-h,o-z)      
c
      dimension ndays(12)
c
      character(3)amon,month(12)
c      
      ndays( 1)=  0 ! 31
      ndays( 2)= 31 ! 29 in Leap Year, else 28
      ndays( 3)= 59 ! 31
      ndays( 4)= 90 ! 30
      ndays( 5)=120 ! 31
      ndays( 6)=151 ! 30
      ndays( 7)=181 ! 31
      ndays( 8)=212 ! 31
      ndays( 9)=243 ! 30
      ndays(10)=273 ! 31
      ndays(11)=304 ! 30
      ndays(12)=334 ! 31
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
      imon=-1   ! Error check
      if(amon.eq.'Jan')imon= 1
      if(amon.eq.'Feb')imon= 2
      if(amon.eq.'Mar')imon= 3
      if(amon.eq.'Apr')imon= 4
      if(amon.eq.'May')imon= 5
      if(amon.eq.'Jun')imon= 6
      if(amon.eq.'Jul')imon= 7
      if(amon.eq.'Aug')imon= 8
      if(amon.eq.'Sep')imon= 9
      if(amon.eq.'Oct')imon=10
      if(amon.eq.'Nov')imon=11
      if(amon.eq.'Dec')imon=12
      if(imon.lt.0
     *  )then
             write(0,104)amon
             stop
      endif
      lday=ndays(imon)+iday
      if(mod(nyr,4)  .eq.0.and.
     *   mod(nyr,100).ne.0.and.
     *   imon.gt.2
     *  )then
             lday=lday+1
      endif
      itod=(ihrs*3600)+(mins*60)+isecs
      ityear=(lday*86400)+itod
c        
      return
c        
104   format('ERROR: Unrecognisable month', a5)
c
      end      
