      program array3D
c
c      Takes the array of colour values, and produces stereo images...
c
      use bmp_comms
      use isocomm
c
      integer,dimension(0:256) :: histo
c
      real (4) adjacent,opposite
      logical lim_change,unscaled
      character(6)seqno
      character(32)colvalname
c
      call defmap(254,7,iret)
      unscaled=.true.
c
  1   continue
c
      read(*,*,end=2)seqno
      colvalname='colour_array.'//seqno
      open(40,file=trim(colvalname),form='unformatted'
     *       ,status='old',err=98)
      read(40,err=97)ixm,izm,limit
      write(*,*)ixm,izm,limit
      read(40)rgbquad
c
      if(unscaled
     *  )then
c
             nxphoto=1+(ixm/2)
             nyphoto=1+(izm/2)
c
             ixbm=2*nxphoto
             iybm=2*nyphoto
c
             allocate(       left3d(ixbm,iybm))
             allocate(      right3d(ixbm,iybm))
             allocate(distance_data(ixbm,iybm))
             allocate(paint  (0:limit+1))
             allocate(mset   (0:ixm+1,0:izm+1))
             allocate(mapdata(  ixm,  izm))
c
             xe=2.0*real(ixm)
             ye=512
             ze=real(izm)
C-EYEPOINT   call eyepoint(a,b,c)
c
c      Put some scaling in...
c
             xmin=huge(xmin)
             ymin=huge(ymin)
             xmax=tiny(xmax)
             ymax=tiny(ymax)
c
             do ix=-ixm/2,ixm/2,ixm
                do iy=0,256,256
                   do iz=-izm/2,izm/2,izm
                      call tanxy(real(ix),real(iy),real(iz),x,y)
                      if(x.gt.xmax)xmax=x
                      if(y.gt.ymax)ymax=y
                      if(x.lt.xmin)xmin=x
                      if(y.lt.ymin)ymin=y
                   enddo
                enddo
             enddo
c
             write(*,111)xmax,xmin,ymax,ymin
             delta_x=xmax-xmin
             delta_y=ymax-ymin
             xscale=real(ixbm)/delta_x
             yscale=real(iybm)/delta_y
             unscaled=.false.
      endif
c
      read(40,err=96,end=93)paint
      read(40,err=96,end=94)mset
      read(40,err=96,end=95)mapdata
      close(40)
c
      bmname='3Dpic_'//seqno
c
      distance_data=huge(x)
      left3d=char(0)
c
      zcentre=izm/2
      xcentre=ixm/2
      ycentre=0
      iymax=0
      iymin=huge(iymin)
      do iz=1,izm
         do ix=1,ixm
            if(ichar(mapdata(ix,iz)).ge.0
     *        )then
                   iy=ichar(mapdata(ix,iz))
               else
                   iy=256+ichar(mapdata(ix,iz))
            endif
c
            lim_change=.false.
            if(iy.lt.iymin
     *        )then
                   iymin=iy
                   lim_change=.true.
            endif
c
            if(iy.gt.iymax
     *        )then
                   iymax=iy
                   lim_change=.true.
            endif
c
         enddo
      enddo
      write(20,107)iymax,iymin
c
      do iz=1,izm
         zp=real(iz)-zcentre
         do ix=1,ixm
            xp=real(ix)-xcentre
c
            if(ichar(mapdata(ix,iz)).ge.0
     *        )then
                   iy=ichar(mapdata(ix,iz))
               else
                   iy=256+ichar(mapdata(ix,iz))
            endif
            if(iy.eq.0
     *        )then
                   iy=255
            endif
            histo(iy)=histo(iy)+1
c
c      Treat this IY as the top of a square pillar based on the pixel
c      at IX, IZ
c
            do iya=iy,iymin,-1
               yp=iya-iycentre
c
               call tanxy(xp,yp,zp,x,y)
               ixphoto=(x-xmin)*xscale
               iyphoto=(y-ymin)*yscale
               if(ixphoto.ge.1.and.ixphoto.le.ixbm.and.
     *            iyphoto.ge.1.and.iyphoto.le.iybm
     *           )then
                      d=pyth(xe,ye,ze,xp,yp,zp)
                      if(d.lt.distance_data(ixphoto,iyphoto)
     *                  )then
                             distance_data(ixphoto,iyphoto)=d
                             if(mapdata(ix,iz).eq.char(0)
     *                         )then
                                    if(iya.eq.iy
     *                                )then
                                           left3d(ixphoto,iyphoto)
     *                                       =char(255)
                                       else
                                           left3d(ixphoto,iyphoto)
     *                                       =char(210)
                                    endif
                                else
                                    left3d(ixphoto,iyphoto)=char(iya)
                             endif
                      endif
               endif
            enddo
         enddo
      enddo
c
      call array2bmp(ixbm,iybm,left3d)
      write(20,108)(mx,histo(mx),mx=0,256)
      call isoflush(20)
      go to 1
c
  2   continue
      stop
c
 93   continue
      write(*,1033)ixm,izm
      stop
c
 94   continue
      write(*,1034)ixm,izm
      stop
c
 95   continue
      write(*,1035)ixm,izm
      stop
c
 96   continue
      write(*,100)ixm,izm
      stop
c
 97   continue
      write(*,101)
      stop
c
 98   continue
      write(*,102)trim(colvalname)
      stop
c
100   format('##### ERROR: CANNOT READ ARRAY'
     *      ,', IXM=',i6,', IZM=',i6)
101   format('##### ERROR: CANNOT READ ARRAY DIMENSIONS')
102   format('##### ERROR: CANNOT OPEN COLOUR_VALUES FILE ',a)
1033  format('##### ERROR: PREMATURE EOF on paint data..'
     *      ,', IXM=',i6,', IZM=',i6)
1034  format('##### ERROR: PREMATURE EOF on mset data..'
     *      ,', IXM=',i6,', IZM=',i6)
1035  format('##### ERROR: PREMATURE EOF on array data..'
     *      ,', IXM=',i6,', IZM=',i6)
104   format( 'IX =',i6,', IY =',i6,', IZ =',i6
     *      ,', XPHOTO=',f8.4,', YPHOTO=',f8.4
     *      ,', Clx=',f6.1,', Cly=',f6.1,', Clz=',f6.1)
1062  format(6f9.3)
107   format('IYMAX=',i4,', IYMIN=',i4)
108   format(2i12)
109   format(/' ##### Sizing complete #####',/)
110   format('PHOTOMAX= ',e12.7)
111   format('Max X=',f6.3,', Min X=',f6.3
     *    ,', Max Y=',f6.3,', Min Y=',f6.3)
c
      end
