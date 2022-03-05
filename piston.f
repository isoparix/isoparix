      program piston
c
c      1. Take a circle of unit radius centred at the origin O(0,0)
c      2. Draw a line (not necessarily a tangent) from the point P(p,0) 
c         to the circumference of the circle at point Q(x,y)
c      3. Let the external angle QOP be theta plus 180 degrees
c      4. Let the angle QPO be phi
c      5. Let the angle PQO be psi (=180-theta-phi)
c
c      For all 0 < theta < 180 degrees:
c         a) find the length l of the line PQ
c         b) find the value of psi
c         c) find the value of dl/d(theta)
c
      implicit real *8 (a-h,o-z)
c
      real(8),dimension (400) :: dldt,degrees
      real(8),dimension (10)  :: deglist,dldtlim
c
      nsteps=360
      r2deg=float(180*113)/355.0
      delta_theta=0.01
      r_delta_theta=1.0/(delta_theta*r2deg)
      semidelta=0.5*delta_theta
      b=710.0/float(113*nsteps)
c
      write(*,102)
      read(*,*)radius
      write(*,103)
      read(*,*)pivot
      write(*,1041)
      read(*,*)or
      write(*,1042)
      read(*,*)pq
c
c     do npivot=250,1000,5
c        pivot=npivot
c           do ncantil=0,100
c              cantil=ncantil
c
c
c
      dldt=0.0
      dldtmin=1000.
      write(4,100)radius,pivot,or,pq,r_delta_theta
      write(*,101)
      do n=nsteps,1,-1
         theta=b*float(n)
         thetb=theta+semidelta
         thetc=theta-semidelta
         call extension(thetc,pivot,radius,or,pq,actorc,phi,px,py)
         if(phi.eq.-1000.)go to 1   ! Broken extension..
         call extension(thetb,pivot,radius,or,pq,actorb,phi,px,py)
         if(phi.eq.-1000.)go to 1   ! Broken extension..
         call extension(theta,pivot,radius,or,pq,actora,phi,px,py)
         if(phi.eq.-1000.)go to 1   ! Broken extension..
c
         stretch=actora-pivot+radius
         dldt(n)=(actorb-actorc)*r_delta_theta
         if(dldt(n).lt.dldtmin)dldtmin=dldt(n)
         dtdl=1./dldt(n)
         qx=radius*cos(theta)
         qy=radius*sin(theta)
         theta=theta*r2deg
         degrees(n)=theta
         write(4,100)px,py,qx,qy,theta,actora
     *              ,dldt(n),phi*r2deg,dtdl,stretch
c        if(stretch.gt.900.)exit
c
  1      continue
c
      enddo
c
      write(4,108)
c
c
c
      dldt_old=dldt(1)
      dldtlim(1)=0.95*dldtmin
      dldtlim(2)=0.90*dldtmin
      dldtlim(3)=0.85*dldtmin
      dldtlim(4)=0.80*dldtmin
      ndeg=0
      deglist=0.0
      do n=2,nsteps
         a=dldt(n)
         do limd=1,4
            if((dldtlim(limd)-a)*(dldtlim(limd)-dldt_old).lt.0.0
     *        )then
                   ndeg=ndeg+1
                   deglist(ndeg)=(degrees(n)+degrees(n-1))/2.0
            endif
         enddo
         dldt_old=a
      enddo
      write(*,107)deglist(5)-deglist(4),radius,pivot,cantil
     *           ,ndeg,(deglist(mx),mx=1,ndeg)
      call flush(4)
c
c
c
c     enddo
c     enddo
      stop
c
100   format(9f9.2,2(f9.3,f9.2),f9.2)
101   format('       OR       PQ       PX       PY    Theta'
     *      ,'   Length    dL/dt      Phi    dt/dL  Stretch'
     *      ,/' ')
102   format('Radius?')
103   format('Pivot distance?')
1041  format('Base cantilever distance?')
1042  format('Floating cantilever distance?')
105   format('     Radius:',f9.2,',     Pivot:',f9.2
     *      ,', Cantilever',f9.2,i6,12f6.1)
106   format(i4,12f12.3)
107   format(f10.3,3f9.2,i6,12f6.1)
108   format(6('   999.00'),/)
c
      end
