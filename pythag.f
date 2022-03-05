      program pythag
c
c      Returns pythagorean distance (x1,y1,z1) to (x2,y2,z2)
c
      implicit real(8) (a-h,o-z)
c
      rad2deg=180.*113./355.
  1   continue
c
      write(*,100)
      read(*,*)xp,yp,zp
      write(*,101)
      read(*,*)xq,yq,zq
c
c      A is point on x-z plane below P
c      B is point on x-z plane below Q
c
      xa=xp
      ya=0.0
      za=zp
c
      xb=xq
      yb=0.0
      zb=zq
c
      ab=sqrt(pyth(xa,ya,za,xb,yb,zb))
      pq=sqrt(pyth(xp,yp,zp,xq,yq,zq))
c
      asa=ab/pq
      aca=(zb-za)/(xb-xa)
c
      asb=sqrt(1.0-asa**2)
      acb=sqrt(1.0-aca**2)
c
      as=rad2deg*acos(asa)
      ac=rad2deg*atan(aca)
c
      write(*,102)pq,ab,asa,as,aca,ac,asb,acb
      go to 1
c
100   format('P(xp,yp,zp)?')
101   format('Q(xq,yq,zq)?')
102   format(/'A is point on x-z plane below P'
     *      ,/'B is point on x-z plane below Q'
     *      ,/'Distance PQ is',f8.2
     *      ,/'Distance AB is',f8.2
     *      ,/'    Angles are',2(f12.5,f6.1),' degrees'
     *      ,/'Triangle sides',  f12.5,f18.5
     *      ,/)
c
      end
