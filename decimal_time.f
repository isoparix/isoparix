      program decimal_time
c      
c
c      Takes in 23 Jun 12 22 11 00 and
c      returns time of day and time of year
c
      implicit real(8) (a-h,o-z)
c
      character(3)amonth
c
  1   continue
      write(*,100)
      read (*,*,end=2)iday,amonth,nyear,ihrs,mins,isecs
c     write(*,102)
      call isotime(iday,amonth,nyear,ihrs,mins,isecs,itod,ityear,lday)
      year=float(nyear)+(float(ityear)/(86400*365.0))
      write(*,104)year,ityear,itod,lday
      write(2,104)year,ityear,itod,lday
      go to 1
c
  2   stop
c
100   format(//'Enter date and time, eg 31 Jul 14 11 53 17')
102   format('Enter time of day - hours mins secs, eg 13 52 15')
104   format('Decimal year:',f12.6,', Year in seconds:',i10
     *       ', Day in seconds:',i6,', Day of year=',i5)
c
      end
