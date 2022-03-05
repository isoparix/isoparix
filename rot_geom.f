      program rot_geom
c
c
c
      implicit real (8) (a-h,o-z)
c
      deg2rad=355.0/(113.0*180.0)  ! 2*pi/360
      rad2deg=1./deg2rad
      thetadeg=51.41684
      phideg=thetadeg/2.0
c
      extension=0.76  !  Threaded rod drive
c
      theta=thetadeg*deg2rad
      phi=theta/2.0
      ctheta=cos(theta)
      stheta=sin(theta)
      cphi=cos(phi)
      sphi=sin(phi)
      tphi=tan(phi)
c
      OB=1.0
      mcount=100
      rmcount=1./float(mcount)
c
c     do n=50,mcount
c        PB=float(n)*rmcount
  1   continue
      write(*,101)
      read(*,*)OB
      if(OB.eq.0.0)stop
c
         r=extension/(2.0*cphi)
         PB=(OB+r)*tphi
         PR=((OB*stheta)-(PB*ctheta))/sphi
         PQ=sqrt((r+OB)**2 + PB**2)
         yP=PB*ctheta
         yO=OB*stheta
         z=(PB*stheta)+(OB*ctheta)
         PO=sqrt(z**2 +(yP-yO)**2)
         sidearm=sqrt(0.5**2 + (yP-yO)**2 + z**2)
         crossbeam=PO*tan(30.0*deg2rad)
c
         write(*,100)PB,r,phideg,extension,PR,PQ,sidearm,crossbeam
      go to 1
c     enddo
c
      stop
c
100   format('Prop=',f8.4,', radius=',f8.4,', phideg=',f6.2
     *      ,', extension=',f8.4,', Min:',f8.4,', Max:',f8.4
     *      ,', sidearm=',f8.4,', crossbeam=',f8.4)
101   format('Enter shaft length in metres')
c
      end
