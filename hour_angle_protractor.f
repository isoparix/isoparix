      program protractor
c
c      Draws a protractor of hour angles in hours and minutes
c
c
      use bmp_comms
c
      implicit real *8 (a-h,o-z)
c
      character(1) text_array(0:19)
      character(20) time_chars
      equivalence (time_chars,text_array)
c
      character(1) black
c
      ndegrees=85
      nsub=4
      ndivs=nsub*ndegrees
c
      rad2deg=float(180*113)/355.0
      deg2rad=1.0/rad2deg
      delta_theta_deg=1.0/float(nsub)     ! 1 div, in degrees
      delta_theta=deg2rad*delta_theta_deg ! 1 div, in radians
c
      call greymap
      call get_font
c
   2  continue
c
      write(*,103)
      read(*,*)linesegs, nwidth ! segments along radius, pixel line width
      if(linesegs.eq.0)stop
c
      demi_w=0.5*float(nwidth)
      ndemi=demi_w
      write(bmname,101)linesegs,nwidth
      als=float(linesegs)
      alsmin=als*sin(float(ndivs)*delta_theta)
c
      ixmin=-1.02*alsmin
      ixmax= 1.02*als
      iymin=-0.02*als
      iymax= 1.02*als
      ixbm=ixmax-ixmin+1
      iybm=iymax-iymin+1
      magtext=linesegs/200
      magnumsa=linesegs/300
      magnumsb=linesegs/400
      line_delta=linesegs/15
      line_init=linesegs/5
c
      allocate(canvas(ixmin:ixmax,iymin:iymax))
      canvas=char(255)
      black =char(0)
c
      canvas(ixmin,       iymin:iymax)=black
      canvas(-ndemi:ndemi,iymin:iymax)=black
      canvas(ixmax,       iymin:iymax)=black
      canvas(ixmin:ixmax,-ndemi:ndemi)=black
      write(8,106)ixmin,ixmax,ndemi,iymin,iymax,linesegs,ixbm,iybm
c
      time_chars='Local'
      level=line_init
      call text_writer(magtext,0,level,ixbm,iybm,time_chars,19)
      time_chars='Hour'
      level=line_init+line_delta
      call text_writer(magtext,0,level,ixbm,iybm,time_chars,19)
      time_chars='Angle'
      level=line_init+(2*line_delta)
      call text_writer(magtext,0,level,ixbm,iybm,time_chars,19)
c
      ihour=6
      do mark=-ndivs,ndivs
c
         theta=delta_theta*float(mark)
         ctheta=cos(theta)
         stheta=sin(theta)
c
         n1=-1
c
         if(mod(mark,60).eq.0
     *     )then
                n1=(4*linesegs)/8   !   Hours
                ihour=ihour-1
                if(ihour.lt.0)ihour=23
                write(time_chars,104)ihour
                nxlab=0.9*float(n1)*stheta
                nylab=0.9*float(n1)*ctheta
      call text_writer(magnumsa,nxlab,nylab,ixbm,iybm,time_chars,1)
                go to 1
         endif
c
         if(mod(mark,30).eq.0
     *     )then
                n1=(5*linesegs)/8   !   Thirty minutes
                time_chars=':30'
                nxlab=0.95*float(n1)*stheta
                nylab=0.95*float(n1)*ctheta
      call text_writer(magnumsb,nxlab,nylab,ixbm,iybm,time_chars,2)
                go to 1
         endif
c
         if(mod(mark,10).eq.0
     *     )then
                n1=(6*linesegs)/8   !   Ten minutes
                go to 1
         endif
c
         if(mod(mark,2 ).eq.0
     *     )then
                n1=(7*linesegs)/8   !   Two minutes
                go to 1
         endif
c
  1      continue
         if(n1.gt.0
     *     )then
                write(4,100)n1,linesegs,stheta,ctheta
                do n=n1,linesegs
                   ax=float(n)*stheta
                   ay=float(n)*ctheta
                   ixm=ax-demi_w  ! Minus
                   ixp=ax+demi_w  ! Plus
                   iym=ay-demi_w
                   iyp=ay+demi_w
                   canvas(ixm:ixp,iym:iyp)=black
                enddo
         endif
c
      enddo
c
      call array2bmp(ixbm,iybm,canvas)
c
      stop
c
100   format(2i6,5f8.2)
101   format('HourAngle_',i5.5,'_',i2.2)
102   format(i4,4f8.4,'  ',a)
103   format('Integer length of radius, integer line width (pixels)?') 
104   format(i2.2) 
106   format('Hour_angle:  ixmin=',i5,', ixmax=',i5,', ndemi=',i3
     *                 ,', iymin=',i5,', iymax=',i5,', linesegs=',i5
     *                  ,', ixbm=',i5,', iybm=' ,i5)
c
      end
