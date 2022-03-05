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
      nsub=4
      ndeg=-50
      ndeg1=(ndeg+10)*nsub
      ndeg2= 90*nsub
c
      rad2deg=float(180*113)/355.0
      deg2rad=1.0/rad2deg
      delta_theta_deg=1.0/float(nsub)     ! 1 div, in degrees
      delta_theta=deg2rad*delta_theta_deg ! 1 div, in radians
c
      call greymap
      call get_font
   2  continue
c
      write(*,103)
      read(*,*)linesegs, nwidth ! segments along radius, pixel line width
      if(linesegs.eq.0)stop
c
      demi_w=0.5*float(nwidth)
      ndemi=demi_w
      write(bmname,101)linesegs,nwidth
      nhla=1*linesegs/4
      nhlb=2*linesegs/4
      nhlc=3*linesegs/4
      nbar=linesegs/100
      magtext=linesegs/200
      magnums=linesegs/300
      line_delta=linesegs/15
      line_init=linesegs/5
c
      als=float(linesegs)
      alsmin=als*sin(float(-ndeg1)*delta_theta)
      ixmin=-1.02*alsmin
      ixmax= 1.02*als
      iymin=-0.03*als
      iymax= 1.03*als
      ixbm=ixmax-ixmin+1
      iybm=iymax-iymin+1
      black=char(0)
c
      allocate(canvas(ixmin:ixmax,iymin:iymax))
      canvas=char(255)
c
c      Create frame
c
      canvas(ixmin:ixmin+nwidth, iymin:iymax)=black
      canvas(-ndemi:ndemi      , iymin:iymax)=black
      canvas(ixmax:ixmax-nwidth, iymin:iymax)=black
      canvas(ixmin:ixmax       ,-ndemi:ndemi)=black
      canvas(ixmin:ixmax       ,linesegs-ndemi:linesegs+ndemi)=black
      write(8,106)ixmin,ixmax,ndemi,iymin,iymax,linesegs,ixbm,iybm
c
      time_chars='Declination'
      call text_writer(magtext,0,line_init,ixbm,iybm,time_chars,19)
c     write(8,107)ndeg1,ndeg2
      do mark=ndeg1,ndeg2
c
         theta=delta_theta*float(mark)
         ctheta=cos(theta)
         stheta=sin(theta)
c
         n1=-1
c
         if(mod(mark,40).eq.0
     *     )then
                n1=(4*linesegs)/8   !   Ten degrees
                ndeg=ndeg+10
                if(ndeg.lt.0)write(time_chars,105)ndeg
                if(ndeg.eq.0)time_chars='0'
                if(ndeg.gt.0)write(time_chars,104)ndeg
                nxlab=0.9*float(n1)*stheta
                nylab=0.9*float(n1)*ctheta
      call text_writer(magnums,nxlab,nylab,ixbm,iybm,time_chars,2)
                go to 1
         endif
c
         if(mod(mark,20).eq.0
     *     )then
                n1=(5*linesegs)/8   !   Five degrees
                go to 1
         endif
c
         if(mod(mark,4).eq.0
     *     )then
                n1=(6*linesegs)/8   !   One degree
                go to 1
         endif
c
         if(mod(mark,2 ).eq.0
     *     )then
                n1=(7*linesegs)/8   !   Half degree
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
101   format('Declination_',i5.5,'_',i2.2)
102   format(i4,4f8.4,'  ',a)
103   format('Integer length of radius, integer line width (pixels)?') 
104   format(i2) 
105   format(i3) 
106   format('Declination: ixmin=',i5,', ixmax=',i5,', ndemi=',i3
     *                 ,', iymin=',i5,', iymax=',i5,', linesegs=',i5
     *                  ,', ixbm=',i5,', iybm=' ,i5)
107   format('Declination range: ndeg1=',i6,', ndeg2=',i6)
c
      end
