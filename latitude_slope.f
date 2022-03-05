      program latitude_slope
c
c      Draws an angle
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
      rad2deg=float(180*113)/355.0
      deg2rad=1.0/rad2deg
c
      call greymap
   2  continue
c
      write(*,103)
      read(*,*)theta,linesegs, nwidth ! theta, segments along radius, pixel line width
      if(linesegs.eq.0)stop
c
      demi_w=0.5*float(nwidth)
      ndemi=demi_w
      write(bmname,101)theta,linesegs,nwidth
      theta=theta*deg2rad
c
      als=float(linesegs)
      iyheight=float(linesegs)/sqrt(2.0)  !  'A'-format paper
      ixmax=linesegs
      black=char(0)
c
      allocate(canvas(0:ixmax,0:iyheight))
      canvas=char(255)
c
c      Create top line
c
      nybase=iyheight/10
      canvas(0:ixmax,nybase-ndemi:nybase+ndemi)=black
c
         ctheta=cos(-theta)
         stheta=sin( theta)
c
         n1=0
                do n=n1,2*linesegs
                   ax=float(n)*stheta
                   ay=float(n)*ctheta
                   ixm=ax-demi_w  ! Minus
                   ixp=ax+demi_w  ! Plus
                   iym=ay-demi_w
                   iyp=ay+demi_w
                   iym=iym+nybase
                   iyp=iyp+nybase
                   if(iyp.gt.iyheight.or.
     *                ixp.gt.ixmax   .or.
     *                ixm.lt.0       .or.
     *                iym.lt.0
     *               )then
                          write(2,107)ixm,ixp,iym,iyp
                      else
                          canvas(ixm:ixp,iym:iyp)=black
                   endif
                enddo
c
      call array2bmp(ixmax+1,iyheight+1,canvas)
c
      stop
c
100   format(2i6,5f8.2)
101   format('Angle_',f0.5,'_',i4.4,'_',i2.2)
102   format(i4,4f8.4,'  ',a)
103   format('Decimal theta, pixel radius,'
     *      ,' pixel line width?') 
104   format(i2) 
105   format(10i8) 
106   format('Declination: ixmin=',i5,', ixmax=',i5
     *                 ,', iymin=',i5,', iymax=',i5
     *                  ,', ixbm=',i5,', iybm=' ,i5)
107   format('Limit:',10i8) 
c
      end
