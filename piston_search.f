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
      real(8),dimension (100) :: dldt,degrees
      real(8),dimension (10)  :: deglist,dldtlim
c
      nsteps=60
      r2deg=float(180*113)/355.0
      delta_theta=0.01
      r_delta_theta=1.0/(delta_theta*r2deg)
      semidelta=0.5*delta_theta
      b=355.0/float(113*nsteps)
c
  1   continue
c
      write(*,102)
      read(*,*)radius
c     write(*,103)
c     read(*,*)pivot
c     write(*,104)
c     read(*,*)cantil
c
      do npivot=250,1000,1
         pivot=npivot
            do ncantil=0,100
               cantil=ncantil
c
c
c
      dldt=0.0
      dldtmin=1000.
      write(*,105)radius,pivot,cantil
c     write(*,101)
      do n=nsteps,1,-1
         theta=b*float(n)
         thetb=theta+semidelta
         thetc=theta-semidelta
         call extension
     *       (thetc,pivot,radius,cantil,actorc,degc,phi,rx,ry,rr,or)
         call extension
     *       (thetb,pivot,radius,cantil,actorb,degb,phi,rx,ry,rr,or)
         call extension
     *       (theta,pivot,radius,cantil,actora,dega,phi,rx,ry,rr,or)
c
         dega=dega*r2deg
         degb=degb*r2deg
         degc=degc*r2deg
c
         if(dega.lt.0.0)dega=180.0+dega
         if(degb.lt.0.0)degb=180.0+degb
         if(degc.lt.0.0)degc=180.0+degc
c
         if(radius.gt.0
     *     )then
                stretch=actora-pivot+radius
            else
                stretch=actora-pivot-radius
         endif
         dldt(n)=(actorb-actorc)*r_delta_theta
         if(dldt(n).lt.dldtmin)dldtmin=dldt(n)
         dtdl=1./dldt(n)
         theta=theta*r2deg
         degrees(n)=theta
c        write(*,100)rr,rx,ry,or,theta,actora
c    *              ,dega,dldt(n),phi*r2deg,dtdl,stretch
c        if(stretch.gt.900.)exit
c
      enddo
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
      enddo
      enddo
      stop
c
100   format(4f9.2,10f12.3)
101   format(7x,'Rr',7x,'Rx',7x,'Ry',7x,'Or',7x,'Theta',6x,'Length'
     *      ,7x,' degx dL/d(theta)',9x,'Phi',' d(theta)/d(L)   Stretch'
     *      ,/)
102   format('Radius?')
103   format('Pivot distance?')
104   format('Cantilever distance?')
105   format(f10.3,' Radius:',f9.2,', Pivot:',f9.2
     *      ,', Cantilever',f9.2,i6,12f6.1)
106   format(i4,12f12.3)
107   format(f10.3,3f9.2,i6,12f6.1)
c
      end
