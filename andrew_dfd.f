      program andrew_dfd    ! Distances From Derby...
c
c      Reads in data pairs and gives offsets from 'Derby'
c
      implicit real(8) (a-h,o-z)
c
c      Calculate offsets for flat-earth based on lon=-1.5, lat=53.0
c      (near Derby....)
c
  1   continue
         write(*,100)
         read(*,*,err=98)alon,alat
         call dfd(alon,alat,x,y)
         write(*,101)x,y
      go to 1
c
      stop
c
 98   continue
      write(*,103)
      go to 1
      stop
c
100   format(/'Enter decimal longitude and latitude')
101   format('Distances From Derby (-1.5,53.0)',2f12.4,' miles')
103   format('ERROR IN INPUT')
c
      end
