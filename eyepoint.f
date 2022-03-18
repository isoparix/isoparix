      subroutine eyepoint(r,theta,phi)
c
      use isocomm
      real (8)r,theta,phi
c
c      r (radius), theta (angle with X-axis), phi (angle with X-Z plane)
c      are all REAL (4) !!
c
c      Creates some fundamental constants about this view
c
      if(phi.eq. 90.)phi= 90.01
      if(phi.eq.270.)phi=270.01
c
c      Eyepoint is at (xe,ye,ze)
c
c     write(0,*)r, theta, phi
      degrad=355./(113.*180.)
c     zalpha=degrad*alpha
      ztheta=degrad*theta
      zphi  =degrad*phi  
c
      ye=r*sin(zphi)
      cp=r*cos(zphi)
      xe=cp*cos(ztheta)
      ze=cp*sin(ztheta)
c     if(cos(zphi).eq.0.0
c    *  )then
c            phi_inv=1000000.
c     else
             phi_inv=sign(1.,cos(zphi))
c     endif

c
      if(r.eq.0.0
     *  )then
             rr2=1000000.
         else
             rr2=1./r**2
      endif
c
      if(cp.eq.0.0
     *  )then
             rd2=1000000.
         else
             rd2=1./cp**2
      endif
c
c      If there are any cubes in existence, we must redefine their
c      screen co-ordinates, as this is a new view
c
             do nc=1,ncubes
                call cubespec(nc)
             enddo
c
      if(check
     *  )then
             write(lchann,106)xe,ye,ze,rd2,rr2,phi_inv
             call isoflush(lchann)
      endif
c
      return
c
106   format('EYEPOINT: XE=',f9.3,', YE=',f9.3,', ZE=',f9.3
     *      ,', RD2=',f9.3,', RR2=',f9.3,', PHI_INV=',f12.1)
c
      end
