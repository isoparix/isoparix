      subroutine tanxy(xp,yp,zp,tanx,tany)
c
c      Returns the tangents to the eyeline in the x- and y-directions
c      of the 'photograph'
c
c      Viewed point is at (xp,yp,zp)
c      Closest point on eyeline to the viewed point is 
c         (xclosest,yclosest,zclosest)
c      Closest point on plane of Y-axis and eyeline to the viewed point
c      is (xn,yn,zn)
c
c
      use isocomm
c
      real(8)cxyz,xclosest,yclosest,zclosest,ec2,cn2,rec2,pn2,cxz
c
c      Compute the co-ordinates of the point on the eyeline that is 
c      closest to the viewed point
c
      cxyz=(xp*xe+yp*ye+zp*ze)*rr2
      xclosest=xe*cxyz
      yclosest=ye*cxyz
      zclosest=ze*cxyz
c
c      Compute the locations of the intersection of the normal to the 
c      W-plane from the point
c
      cxz =(xp*xe+zp*ze)*rd2
      xn=xe*cxz
      yn=yp
      zn=ze*cxz
c
       d2=(xp-xe)**2      +(yp-ye)**2      +(zp-ze)**2
      pn2=(xn-xp)**2      +(yn-yp)**2      +(zn-zp)**2
      cn2=(xclosest-xn)**2+(yclosest-yn)**2+(zclosest-zn)**2
      ec2=(xclosest-xe)**2+(yclosest-ye)**2+(zclosest-ze)**2
c
      if(ec2.eq.0.0
     *  )then
             rec2=1000000.0
         else
             rec2=1./ec2
      endif
c
c     write(*,100)xe,ye,ze,pn2,cn2,rec2,pn2*rec2,cn2*rec2,cxz,cxyz
      if(zp*xe.lt.xp*ze
     *  )then
             tanx= phi_inv*sqrt(pn2*rec2)
         else
             tanx=-phi_inv*sqrt(pn2*rec2)
      endif
c
      if(yp.gt.yclosest
     *  )then
             tany=-phi_inv*sqrt(cn2*rec2)
         else
             tany= phi_inv*sqrt(cn2*rec2)
      endif
c
      if(check
     *  )then
             write(lchann,1061)xp,yp,zp,xclosest,yclosest,zclosest
     *                   ,xn,yn,zn,ec2,rec2,cn2,tanx,tany
             call isoflush(lchann)
      endif
c
      return
c
100   format(12f9.3)
1061  format(/'XP=',f8.1,', YP=',f8.1,', ZP=',f8.1
     *      ,/'XC=',f8.1,', YC=',f8.1,', ZC=',f8.1
     *      ,/'XN=',f8.1,', YN=',f8.1,', ZN=',f8.1
     *      ,/'   EC2=',f20.3,',   REC2=',f20.3,', CN2=',f20.3
     *      ,/'TAN(X)=',f20.3,', TAN(Y)=',f20.3)
c
      end
