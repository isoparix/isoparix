      subroutine lightgen
     *                (xe,ye,ze,xcv,ycv,zcv,lightname,ncpus,r,theta,phi)
c
c      Generates a light parallel to the direction of view
c
      implicit real(8) (a-h,o-z)
      character(40)lightname
c
      deg2rad=355./(180.0*113.0)
      rad2deg=1./deg2rad
c
      xa=xe-xcv
      ya=ye-ycv
      za=ze-zcv
c
      r=sqrt(xa**2+za**2)
c
      theta=acos(xa/r)   ! Returns 0 < theta < 180
      if(za.lt.0.0)theta=-theta
c
      phi=atan(ya/r)
c
      write(*,*)ncpus,rad2deg,theta,phi,rad2deg*theta,rad2deg*phi
      theta_orig=rad2deg*theta
        phi_orig=rad2deg*phi
      do np=1,ncpus
         write(lightname,1061)np ! Original light file for this stream
         open(80,file=trim(lightname),form='formatted',status='unknown')
            write(80,118)theta_orig, phi_orig
         close(80)
      enddo
c
      return
c
1061  format('ls.',i3.3)   !  Light stream
118   format(f8.3,/f8.3)
c
      end
