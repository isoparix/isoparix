      program solar_master
c
      use bmp_comms
      use ip_comms
c
c      Plots energy from a solar panale
c
      character (4) pictype
      character (20) char_thresh
c
      real(8)latitude,lambda
c
      real(8),allocatable,dimension(:) :: daily
c
      logical shad
c
      character (23)filename
      character (40)label_txt(12)
      character (1) label_chars(0:39,0:11)
      equivalence(label_txt,label_chars)
c
      call xxstatmap
c
      pi=355./113.
      two_pi=2.0*pi
      deg2rad=pi/180.      ! 2*pi/360
      rad2deg=180./pi
c
      ixdim=1098
      idx=ixdim/366
      isx=idx-1
      iydim=768
      iydimh=iydim/2
      iydimq=iydim/4
      rixdim=1./float(ixdim)
      riydim=1./float(iydim)
      dtime=24.0*riydim
      allocate(picdata(ixdim,iydim))
      allocate(daily(ixdim))
c
      ixelev=366
      iyelev=180
      ixsol=ixdim-ixelev
      iysol=iydim-iyelev
c
c
      call getarg(1,char_thresh)
      read(char_thresh,*,end=99,err=99)latitude
      phi=deg2rad*latitude
      sin_phi=sin(phi)
      cos_phi=cos(phi)
c
      call getarg(2,char_thresh)
      read(char_thresh,*,end=99,err=99)elevation
      beta=deg2rad*elevation
      sin_beta=sin(beta)
      cos_beta=cos(beta)
c
      call getarg(3,char_thresh)
      read(char_thresh,*,end=99,err=99)azimuth
      gammb=deg2rad*azimuth
      sin_gamma=sin(gammb)
      cos_gamma=cos(gammb)
c
      call getarg(4,char_thresh)
      read(char_thresh,*,end=99,err=99)shad
c
      picdata=char(0)
      picdata(ixsol:ixdim,iysol:iydim)=char(255)
c
c      Grid lines on solar elevation chart
c
      a=float(ixelev)/12.0
      do k=1,12
         ix=a*float(k)
         picdata(ix+ixsol,iysol:iydim)=char(1)
      enddo
c
      do iy=iysol,iydim,iyelev/6
         picdata(ixsol:ixdim,iy)=char(1)
      enddo
c
      picdata(ixdim-(ixelev/2),iysol:iydim)=char(254)
      picdata(ixsol:ixdim,iydim-(iyelev/2))=char(254)
c
      id=1
      shadow=rad2deg*atan(5.5/14.0)
      sin_epsilon=sin(23.44*deg2rad)
      total_insolation=0.
      nsun=0
      npanel=0
      npoints=0
      daily=0
      do itod=1,iydim,id
         atod=riydim*float(itod)
         omega=(atod*two_pi)-pi
         solar_azimuth=atod*360.0
         sin_omega=sin(omega)
         cos_omega=cos(omega)
         do idate=1,ixdim,idx
            npoints=npoints+1
            ay   =(rixdim*float(idate))
            ayear=ay-0.25
            lambda=ayear*two_pi
            delta=asin(sin(lambda)*sin_epsilon)
            sin_delta=sin(delta)
            cos_delta=cos(delta)
            sin_solelev=(cos_delta*cos_phi*cos_omega)
     *                          +(sin_delta*sin_phi)
            solar_elevation=rad2deg*asin(sin_solelev)
            if(itod.eq.iydimh.or.itod.eq.iydimq
     *        )then
                   iye=solar_elevation+90.
                   ixe=ay*float(ixelev)
                   picdata(ixsol+ixe,iydim-iye)=char(128)
            endif
c
            cos_theta=-(sin_beta*sin_gamma*cos_delta*sin_omega)
     *                +(sin_beta*cos_gamma*sin_delta*cos_phi)
     *                -(sin_beta*cos_gamma*cos_delta*sin_phi*cos_omega)
     *                +(cos_beta*cos_delta*cos_phi  *cos_omega)
     *                +(cos_beta*sin_delta*sin_phi)
c
            if(cos_theta.gt.1.0
     *        )then
                   cos_theta=1.0
            endif
c           write(8,101)atod,ayear,solar_elevation/deg2rad,cos_theta
            if(solar_elevation.gt.0.
     *        )then
                   nsun=nsun+1
                   picdata(idate:idate+isx,itod)=char(1)
                   if(cos_theta.gt.0
     *               )then
                          if(solar_elevation.lt.shadow
     *                     .and.solar_azimuth.lt.160.0   ! By observation
     *                     .and.shad
     *                      )then
                                 isa=isx-1
                                 npx=0
                                 ti=0.
                             else    
                                 isa=isx
                                 npx=1
                                 ti=cos_theta
                          endif
                          npanel=npanel+npx
                          daily(idate:idate+isx)
     *                   =daily(idate:idate+isx)+ti
                          picdata(idate:idate+isa,itod)
     *                               =char(int(254.0*cos_theta))
                          total_insolation=total_insolation+ti
c
c      Contour grid
c
                          if(
     *                      (cos_theta.gt.0.2.and.cos_theta.lt.0.21).or.
     *                      (cos_theta.gt.0.4.and.cos_theta.lt.0.41).or.
     *                      (cos_theta.gt.0.6.and.cos_theta.lt.0.61).or.
     *                      (cos_theta.gt.0.8.and.cos_theta.lt.0.81).or.
     *                      (cos_theta.gt.0.9.and.cos_theta.lt.0.905)
     *                      )then
                                 picdata(idate:idate+isx,itod)=char(255)
                          endif
                   endif
            endif
c
         enddo
      enddo
c
      panel_percent=100.0*total_insolation/float(nsun)
      panel_insolation=total_insolation*dtime
      daily_peak=maxval(daily)*dtime
c
      write(bmname,  104)int(latitude),int(elevation),int(azimuth),shad
      write(filename,105)int(latitude),int(elevation),int(azimuth),shad
      open(12,file=filename,status='unknown')
      write(12,       102)latitude,elevation,azimuth
     *           ,panel_insolation,panel_percent,daily_peak
      close(12)
      write(label_txt,102)latitude,elevation,azimuth
     *           ,panel_insolation,panel_percent,daily_peak
c
c      Grid lines on main chart
c
      a=float(ixdim)/12.
      do k=1,12
         ix=a*float(k)
c        do iy=1,iydim
c           if(ichar(picdata(ix,iy)).lt.2
c    *        )then
c                  picdata(ix,iy)=char(255)
c              else
c                  picdata(ix,iy)=char(0)
c           endif
c        enddo
         picdata(ix,1:iydim)=char(255)
      enddo
      picdata(1,1:iydim)=char(255)
c
      do iy=1,iydim,32
c        do ix=1,ixdim
c           if(ichar(picdata(ix,iy)).lt.2
c    *        )then
c                  picdata(ix,iy)=char(255)
c              else
c                  picdata(ix,iy)=char(0)
c           endif
c        enddo
         picdata(1:ixdim,iy)=char(255)
      enddo
      picdata(1:ixdim,iydim)=char(255)
c
      call label_maker(1,8,13,ixdim,iydim,picdata,label_chars,39,11)
c
c      Plot daily input
c
      do ix=1,ixdim
         iy=int(daily(ix))
         if(iy.gt.0.0
     *     )then
                iy=iydim-iy
                iy1=amax0(1,iy-1)
                iy2=amin0(iydim,iy+1)
                picdata(ix,iy1:iy2)=char(254)
          endif
      enddo
c
      call array2bmp(ixdim,iydim,picdata)
c
      stop
c
 99   continue
      write(0,200)
      stop
c
100   format(24a1)
101   format(10f12.6)
102   format( 'Panel performance by day and time'
     *      ,/'Latitude        ',f5.2
     *      ,/'Panel elevation ',f4.1,' (from horizontal)'
     *      ,/'Panel azimuth '  ,f6.1,' (South=180)'
     *      ,/'Panel hours  '   ,f7.1,' (',f4.1,' per cent)'
     *      ,/'X: Year - summer solstice at centre'
     *      ,/'Y: Time of day - midnight at top'
     *      ,/
     *      ,/'    Red: Full intensity (normal to sun)'
     *      ,/'    Daily chart peak:',f5.1,' full hours'
     *      ,/'    Copyright John Watts 2012'
     *      ,/'Sproul,Renew Energy 32,1187-1205,2642'
     *      )
104   format('solar',3('_',i3.3),'_',l1)
105   format('solar',3('_',i3.3),'_',l1,'.dat')
c
200   format('solar_master needs four parameters:'
     *     ,/'     Local latitude'
     *     ,/'     Panel elevation from horizontal'
     *     ,/'     Panel azimuth'
     *     ,/'     Shadow (T/F)'
     *      )
c
      end
