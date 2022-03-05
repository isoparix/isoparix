      program pivot_graph
c
c      1. Take a circle of unit radius centred at the origin O(0,0)
c      2. Draw a line (not necessarily a tangent) from the point P(px,py) 
c         to the circumference of the circle at point Q(x,y)
c      3. Let the angle between OQ and the x-zaxis be theta
c      4. Let the angle QPO be phi
c      5. Let the angle PQO be psi (=180-theta-phi)
c      6. Let S be a fixed point on the x-axis at (sx,0)
c      7. Let R be a point that lies on the projection of line OQ
c         at a distance rs from S
c
c
c      For all -theta_lim < theta < theta_lim degrees:
c         a) find the length l of the line PQ
c         b) find the value of psi
c         c) find the value of dl/d(theta)
c
      implicit real *8 (a-h,o-z)
c
      real(8),allocatable,dimension (:) :: points_array,points_array_b
c
      character(30) series_name,series_name_b,graph_name
      character(6) colour_name(0:5)
c
      logical cantilever
c
      colour_name(0)='Red     '
      colour_name(1)='Aqua    '
      colour_name(2)='Blue    '
      colour_name(3)='Fuchsia '
      colour_name(4)='Lime    '
      colour_name(5)='Yellow  '
c
      graph_name='Rotation_rate_vs_extension'
      ndegrees=70
c
      ntotal=1+(2*ndegrees)
      allocate(points_array  (2*ntotal))
      allocate(points_array_b(2*ntotal))
c
      rad2deg=float(180*113)/355.0
      deg2rad=1.0/rad2deg
      theta_lim=float(ndegrees)*deg2rad     ! radians
      delta_theta_deg=1.0      ! 1 degree
      delta_theta=deg2rad      ! 1 degree, in radians
      semidelta=0.01*delta_theta   !  1/100th of a degree, in radians
c
c     write(*,102)
c     read(*,*)cantilever
      radius=1.0        ! Unit radius
c
c     write(*,103)radius
c     read(*,*)sx,sy,sr
c
      xmin=-ndegrees
      xmax= ndegrees
      npstanza=0
      ymin=0.5
      ymax=1.2
      call grf_header(xmin,xmax,ymin,ymax,graph_name)
      nfillcol=-1
      series_name_b='Rod travel'
      psi=0.
      write(*,109)
      read(*,*)psi
      psi=psi*deg2rad
      px1= 1.0
      py1=-3.0
      angle_p =-atan(px1/py1)   ! Between Y-axis and OP
      radius_p=sqrt(px1**2+py1**2)
      px=px1
      py=py1
c     px= radius_p*sin(angle_p+psi)
c     py=-radius_p*cos(angle_p-psi)
c
      write(*,*)radius_p,angle_p*rad2deg
c
      do nx=0,5
         nfillcol=nfillcol+1
         sx1=-.7+(0.05*float(nx))
         nlinecol=-1
         do ny=3,8
            nlinecol=nlinecol+1
            sr=1.0+(0.02*float(ny))-sx1
c
c      Displace sx,sy and qxa,qxb by angle psi
c
               radius_s=sx1
               sx=radius_s*cos(psi)
               sy=radius_s*sin(psi)
c
            write(series_name,101)sx,sy,sr,px,py
            np=0
            npstanza=npstanza+1
            angle_sqp_min= 10000.
            angle_sqp_max=-10000.
            spoke_min= 10000.
            spoke_max=-10000.
            do n=-ndegrees,ndegrees
c
               theta=delta_theta*float(n)
               thetb=theta+semidelta
               thetc=theta-semidelta
c
               call fb(theta,sx,sy,sr,qxa,qya)
c
               spoke=sqrt(qxa**2+qya**2)
               if(spoke.gt.spoke_max)spoke_max=spoke
               if(spoke.lt.spoke_min)spoke_min=spoke
c
               t=rad2deg*theta
               tr=sqrt((sx-qxa)**2+(sy-qya)**2)
               write(4,800)t,sx,sy,sr,qxa,qya,tr,spoke
800            format(8f8.3)
c
               call fb(thetb,sx,sy,sr,qxb,qyb)
               call fb(thetc,sx,sy,sr,qxc,qyc)
c
               rod_a=sqrt((px-qxa)**2+(py-qya)**2)
               rod_b=sqrt((px-qxb)**2+(py-qyb)**2)
               rod_c=sqrt((px-qxc)**2+(py-qyc)**2)
c
               dldt=(rod_b-rod_c)/(2.0*semidelta)
               theta_deg=theta*rad2deg
               angle_sqp=rad2deg*acos(cosrule(sx,sy,qxa,qya,px,py))
               if(angle_sqp.lt.angle_sqp_min)angle_sqp_min=angle_sqp
               if(angle_sqp.gt.angle_sqp_max)angle_sqp_max=angle_sqp
c
               if(np.eq.0
     *           )then
                      rod_first=rod_a
                  else
                      rod_last =rod_a
               endif
c
               np=np+1
               points_array  (np)=n
               points_array_b(np)=n
               np=np+1
               points_array  (np)=dldt
               points_array_b(np)=rod_a-rod_first
c
            enddo
            sqpmean=.5*(angle_sqp_max+angle_sqp_min)
            sqpdiff=    angle_sqp_max-angle_sqp_min 
            write(*,1011)trim(series_name),rod_last-rod_first
     *                  ,spoke_min,spoke_max,spoke_max-spoke_min
     *                  ,rod_first, rod_last,sqpmean,sqpdiff
c
            nsize=0
            nvisible=0
            call grf_points(0,npstanza,np,points_array
     *                     ,colour_name(nfillcol)
     *                     ,colour_name(nlinecol)
     *                     ,nsize,nvisible,series_name)
         enddo
      enddo
c
      call grf_trailer(npstanza)
      stop
c
100   format(3f8.4)
101   format('S',2f5.2,' R',f5.2,' P',2f5.2)
1011  format(a,', travel dist=',f6.3
     *      ,', spoke=',2f6.3,', travel=',f6.3
     *,', lengths=',2f6.3,2f6.1)
102   format('Cantilevered? (T or F)')
103   format('Wheel centre X, Y, wheel radius?'
     *      ,'  (Note: base radius=',f6.3,')')
1041  format('Base cantilever distance?')
1042  format('Floating cantilever distance?')
105   format('     Radius:',f9.2,',     Pivot:',f9.2
     *      ,', Cantilever',f9.2,i6,12f6.1)
106   format(i4,12f12.3)
107   format(f10.3,3f9.2,i6,12f6.1)
108   format(6('   999.00'),/)
109   format('Displacement angle?')
c
      end
c
c
c
      subroutine fb(theta,sx,sy,sr,qx,qy)
c
      implicit real *8 (a-h,o-z)
c
      ca=1./cos(theta)**2
      cb=-2.0*(sx+(sy*tan(theta)))
      cc=sx**2+sy**2-sr**2
c
      cd=cb**2-(4.0*ca*cc)  ! b-squared -4ac
c
      if(cd.ge.0.0   !  Positive roots
     *  )then
             cd=sqrt(cd)      ! Root of 'b-squared - 4ac'
             car=1./(2.0*ca)   ! ...all over 2a
             qx =(-cb-cd)*car  ! -b - root of 'b-squared - 4ac', all over 2a
             qx1=(-cb+cd)*car  ! -b + root of 'b-squared - 4ac', all over 2a
         else
             qx=0.
             qy=0.
             return
      endif
c
      if(qx1.gt.0.0)qx=qx1   !  Return positive root
      qy=qx*tan(theta)
c
      return
      end
