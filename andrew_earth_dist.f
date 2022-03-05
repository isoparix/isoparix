      program andrew_earth_dist
c
      implicit real(8) (a-h,o-z)
c
      character(10) units
      dimension x(2),y(2)
c
c      Reads in lon/lat and delivers surface distance`
c
c     radius=6378.1D+00 ! Kilometres
c     units ='kilometres'
      radius=3963.2D+00 ! Miles
      units ='miles     '
c
  1   continue
         read(*,*)x(1),y(1),x(2),y(2)
         d=sphere_dist(x(1),y(1),x(2),y(2),radius)
         write(*,102)x(1),y(1),x(2),y(2),d,units
      go to 1
c
 99   stop
c
100   format(a10,2i6,2f20.14)
102   format('Spherical surface distance between (',f8.3,',',f8.3,')'
     *                  ,' and (',f8.3,',',f8.3,')'
     *       ,' is',f8.3,' ',a10)
200   format(2(4f30.18,/))
      end
