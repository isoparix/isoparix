      program elevation
c
c   OB is distance from pivot to base of spigot
c   PB is length of spigot
c
      implicit real(8) (a-h,o-z)
c
      real(8),dimension(500) :: array_dphidl,array_phi
     *                         ,array_dpsidl,array_psi
c
      character(30)series_name,graph_name
      character(8)fillcol,linecol
c
c     theta_lat=51.4168
      deg2rad=355.0/(113.0*180.)
      r=500.00
c
  1   continue
      write(*,100)
      read(*,*)ob,pb,theta_lat,section
c
      theta_equator=90-theta_lat
      rad_equator=theta_equator*deg2rad
      theta=theta_lat*deg2rad
      write(*,*)theta
      costheta=cos(theta)
      sintheta=sin(theta)
c
      op2=ob**2+pb**2
      op=sqrt(op2)
      pw=ob*sintheta-pb*costheta
      pob=atan(pb/ob)
      pow=asin(pw/op)
      rw=ob*costheta+pb*sintheta-r
      pr=sqrt(pw**2+rw**2)
      pq=sqrt((r+ob)**2+pb**2)
      write(*,102)pr,pq,r,pq-pr
      rls=1./float(lsint)
      phi_old=-999.0
c      
      shaftmax =-1000000.
      phimax   =-1000000.
      psimax   =-1000000.
      dphidlmax=-1000000.
      dpsidlmax=-1000000.
c      
      shaftmin = 1000000.
      phimin   = 1000000.
      psimin   = 1000000.
      dphidlmin= 1000000.
      dpsidlmin= 1000000.
c
      lsmin=ceiling(op-r)       !  floor(pr)
      lsmax=floor  (op+r)       !  pq
      lsint=10
      ncoord=-1
      do ls=lsmin,lsmax,lsint
            shaft=ls
            pos=acos((op2+r**2-shaft**2)/(2*op*r))
            phi=pos-pow-rad_equator
            psi=atan(sin(phi)/(cos(phi)-section))
            phi=phi/deg2rad
            psi=psi/deg2rad
            write(*,103)ls,shaft,phi,psi
            if(phi_old.gt.-999.0
     *        )then
                   ncoord=ncoord+2
c
                   dpsidl=(psi-psi_old)*rls
                   array_dpsidl(ncoord  )=psi
                   array_dpsidl(ncoord+1)=dpsidl
                   array_psi(ncoord  )=shaft
                   array_psi(ncoord+1)=psi
c
                   dphidl=(phi-phi_old)*rls
                   array_dphidl(ncoord  )=phi
                   array_dphidl(ncoord+1)=dphidl
                   array_phi(ncoord  )=shaft
                   array_phi(ncoord+1)=phi
c
                   write(4,103)ncoord,array_phi(ncoord)
     *                               ,array_phi(ncoord+1)
     *                               ,array_psi(ncoord+1)
                   write(8,103)ncoord,array_dphidl(ncoord)
     *                               ,array_dphidl(ncoord+1)
     *                               ,array_dpsidl(ncoord+1)
            endif
            if( phi  .gt.phimax)      phimax=phi
            if( phi  .lt.phimin)      phimin=phi
            if(dphidl.gt.dphidlmax)dphidlmax=dphidl
            if(dphidl.lt.dphidlmin)dphidlmin=dphidl
            if(shaft .gt.shaftmax)  shaftmax=shaft
            if(shaft .lt.shaftmin)  shaftmin=shaft
            phi_old=phi
            psi_old=psi
      enddo
      ncoord=ncoord+1
c
      write(*,104)phimin,phimax,dphidlmin,dphidlmax,shaftmin,shaftmax
      fillcol='Red     '
      linecol='Blue    '
c
c      subroutine grf_header(xmin,xmax,ymin,ymax,graph_name)
c
      graph_name='dpsi_by_dl'
      series_name='Rate of Change'
      call grf_header(phimin,phimax,dphidlmin,dphidlmax,graph_name)
      call grf_points(0,1,ncoord,array_dpsidl(1:ncoord)
     *               ,fillcol,linecol,1,1,series_name)
      call grf_trailer(1)
c
      graph_name='PSI_vs_length'
      series_name='psi vs shaft length'
      call grf_header(shaftmin,shaftmax,phimin,phimax,graph_name)
      call grf_points(0,1,ncoord,array_psi(1:ncoord)
     *               ,fillcol,linecol,1,1,series_name)
      call grf_trailer(1)
c
      graph_name='dphi_by_dl'
      series_name='Rate of Change'
      call grf_header(phimin,phimax,dphidlmin,dphidlmax,graph_name)
      call grf_points(0,1,ncoord,array_dphidl(1:ncoord)
     *               ,fillcol,linecol,1,1,series_name)
      call grf_trailer(1)
c
      graph_name='Elevation_vs_length'
      series_name='phi vs shaft length'
      call grf_header(shaftmin,shaftmax,phimin,phimax,graph_name)
      call grf_points(0,1,ncoord,array_phi(1:ncoord)
     *               ,fillcol,linecol,1,1,series_name)
      call grf_trailer(1)
c
      stop
100   format('Enter OB, PB, theta_lat (degrees), a')
101   format('Shaft=',f8.1,' Elev=',f6.1,' degrees,  dphidl=',f0.6)
102   format(/'Min length:',f8.1,', max length:',f8.1,', r=',f6.1
     *      ,', difference:',f8.1,/)
103   format(i8,3e14.5)
104   format(6e12.5)
      end
