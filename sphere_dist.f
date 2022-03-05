      real(8) function sphere_dist(alon,alat,blon,blat,radius)
c
c      Returns distance on surface of sphere radius r between 
c      lonlatA and lonlatB.    Lat and lon are given in degrees
c
      implicit real(8) (a-h,o-z)
c
      degrad=3.14159265358979D+00/180.0D+00
      ralon=alon*degrad
      rblon=blon*degrad
      ralat=alat*degrad
      rblat=blat*degrad
c     write(*,200)alon,alat,blon,blat,ralon,ralat,rblon,rblat
c
      sphere_dist=radius*(dacos(
     *                       (dsin(ralat)*dsin(rblat))+
     *                       (dcos(ralat)*dcos(rblat)*dcos(ralon-rblon))
     *                         )
     *                   )
c     write(*,*)sphere_dist,radius
c
200   format(2(4f30.18,/))
c
      end
