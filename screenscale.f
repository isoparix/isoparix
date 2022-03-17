      subroutine screenscale(itheta,iphi)
c
      use isocomm
c
c      Set scale and offsets of picture
c
      xmin=huge(xmin)
      ymin=huge(ymin)
      xmax=0.
      ymax=0.
c
      ixmh=ixm/2
      iymh=iym/2
c
      diagonal=sqrt(real(ixm*ixm+iym*iym))
c
      do iph=0,89,89
         do ith=0,360,2
           call eyepoint(diagonal,real(ith,8),real(iph,8))
            do i=1-ixmh,ixm-ixmh,ixm-1
               xp=i
               do j=0,100,100
                 yp=j
                 do k=1-iymh,iym-iymh,iym-1
                    zp=k
                    call tanxy(xp,yp,zp,xphoto,yphoto)
                    if(xphoto.gt.xmax)xmax=xphoto
                    if(yphoto.gt.ymax)ymax=yphoto
                    if(xphoto.lt.xmin)xmin=xphoto
                    if(yphoto.lt.ymin)ymin=yphoto
                 enddo
               enddo
            enddo
         enddo
      enddo
c
c      call scaler
c
c      Sets up the scaling and offsets to translate photo results to
c      screen positioncall  x/y max/min are the observed limits of the
c      scene.
c
      xscale=(xmax-xmin)/real(ixm)
      yscale=(ymax-ymin)/real(iym)
      scale=amax1(xscale,yscale)
      xoffset=ixmh
      yoffset=iymh
c
      if(check
     *  )then
             write(lchann,100)ixm,iym,ixmh,iymh,xmax,xmin,ymax,ymin
     *                   ,xscale,yscale,scale,xoffset,yoffset
             call isoflush(lchann)
      endif
c
c      Make the defined plane
c
      call eyepoint(diagonal,real(itheta),real(iphi))
c
      return
c
100   format(/'SCREENSCALE:'
     *     /,'IXM, IYM            ',2i18
     *     /,'IXMH,IYMH           ',2i18
     *     /,'XMAX,XMIN,YMAX,YMIN ',4f18.2
     *     /,'XSCALE, YSCALE,SCALE',3f18.2
     *     /,'X-offset, Y-offset  ',2f18.2,/)
      end
