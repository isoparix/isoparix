      program tmp_calc
c      
c      Something to calculate whatever the current problem is....
c      
      implicit real(8) (a-h,o-z)
      radius=512.0
      a     =1.25*25.4  ! Half width of parallel shape that will be
c                         rotated with radius r
      deg2rad=355.0/(113.0*180.0)
   1  continue
      do ntheta=0,75,15
         rad_theta=float(ntheta)*deg2rad
         pq=radius*tan(rad_theta)
         qr=a/cos(rad_theta)
         d=pq+qr
         write(*,101)ntheta,pq,qr,d,2*d
      enddo
c
100   format('theta in degrees?')
101   format(i6,4f10.1)
c
      end

