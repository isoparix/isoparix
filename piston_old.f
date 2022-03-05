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
      nsteps=60
      r2deg=float(180*113)/355.0
      delta_theta=0.01
      a=0.5/delta_theta
      b=1.0/float(113*nsteps)
c     p=87.84
c
  1   continue
c
      write(*,102)
      read(*,*)r
      write(*,103)
      read(*,*)p
      s=(r**2)+(p**2)
      write(*,101)
c
      do n=0,nsteps
         theta=b*float(355*n)
         tl=2*p*r*cos(theta-delta_theta)
         tc=2*p*r*cos(theta)
         th=2*p*r*cos(theta+delta_theta)
c
         if(s-tl.ge.0.0.and.
     *      s-tc.ge.0.0.and.
     *      s-th.ge.0.0
     *     )then
                extension=sqrt(s-tc)
                dldt=(sqrt(s-th)-sqrt(s-tl))*a
                dtdl=r2deg/dldt
                phi=atan((r*sin(theta))/((r*cos(theta))-p))
                psi=theta+phi
                stretch=extension-p+r
                write(*,100)r,p,theta*r2deg,extension,dldt
     *                     ,phi*r2deg,psi*r2deg,dtdl,stretch
            else
                write(*,100)r,p,theta
         endif
c
         if(stretch.gt.900.)exit
c
      enddo
      go to 1
      stop
c
100   format(10f12.3)
101   format(/6x,'Radius',7x,'Pivot',7x,'Theta',6x,'Length'
     *      ,' dL/d(theta)',9x,'Phi',9x,'Psi',' d(theta)/d(L)'
     *      ,'   Stretch',/)
102   format('Radius?')
103   format('Pivot distance?')
c
      end
