      program canted
c
c      Displays cantilevered rotational action
c
      use isocomm
      use bmp_comms
c
      integer(4),dimension(400) :: iqx,iqy,ipx,ipy,iactr,idldt,ithet
      real(8),   dimension(400) :: px,py,qx,qy,theta,actuator,phi,dldt
c
      check=.true.
      lchann=10
c
c      Read in the details of the colour map
c
      call defmap(254,7,iret)
c
      numtasks=8
c
      read(4,101)radius,pivot,cantilever
      write(*,101)radius,pivot,cantilever
c
      ipivot=pivot+.5
      icant=cantilever+.5
      iradius=radius+.5
      write(*,*)iradius,ipivot,icant
c
      npos=0
c
      thetmin= 100000.
      thetmax=-100000.
      dldtmin= 100000.
      dldtmax=-100000.
      actuatormin= 100000.
      actuatormax=-100000.
      xmin=-ipivot
      xmax=-1000000.
      ymin=0.0
      ymax=-1000000.
c
      read(4,101)a1,a2,a3,a4,a5,a6,a7,a8
   1  continue
      npos=npos+1
c
      read(4,101)px(npos),py(npos),qx(npos),qy(npos)
     *         ,theta(npos),actuator(npos),dldt(npos),phi(npos)
      itest=px(npos)+0.5
      if(itest.eq.999
     *  )then
             npos=npos-1
             go to 2
      endif
c
      if(actuator(npos).gt.actuatormax)actuatormax=actuator(npos)
      if(actuator(npos).lt.actuatormin)actuatormin=actuator(npos)
      if(dldt(npos).gt.dldtmax)dldtmax=dldt(npos)
      if(dldt(npos).lt.dldtmin)dldtmin=dldt(npos)
      if(theta(npos).gt.thetmax)thetmax=theta(npos)
      if(theta(npos).lt.thetmin)thetmin=theta(npos)
c
      if(px(npos).gt.xmax)xmax=px(npos)
      if(qx(npos).gt.xmax)xmax=qx(npos)
      if(px(npos).lt.xmin)xmin=px(npos)
      if(qx(npos).lt.xmin)xmin=qx(npos)
      if(py(npos).gt.ymax)ymax=py(npos)
      if(qy(npos).gt.ymax)ymax=qy(npos)
      if(py(npos).lt.ymin)ymin=py(npos)
      if(qy(npos).lt.ymin)ymin=qy(npos)
      go to 1
c
   2  continue
c
      xdiff=xmax-xmin
      ydiff=ymax-ymin
      ixm=xdiff+.5
      iym=ydiff+.5
      ixcentre=-xmin
      iycentre= ymax 
      write(*,*)ixm,iym,xmax,xmin,ymax,ymin,thetmin,thetmax
     *         ,actuatormax,actuatormin,dldtmax,dldtmin
c
      actrscale=.8*ydiff/(actuatormax-actuatormin)
      dldtscale=.8*ydiff/(dldtmax-dldtmin)
      thetscale=xdiff/(thetmax-thetmin)
c
      iactr=((actuator-actuatormin)*actrscale)+.5
      idldt=((dldt-dldtmin)*dldtscale)+.5
      ithet=((theta-thetmin)*thetscale)+.5
c
      iactr=(8*iym/10)-iactr
      idldt=(8*iym/10)-idldt
c
         ipx=px-xmin
         iqx=qx-xmin
         ipy=float(iym)+ymin-py
         iqy=float(iym)+ymin-qy
c
      do n=1,npos
         write(8,102)ithet(n),iactr(n),idldt(n)
     *              ,ipx(n),ipy(n),iqx(n),iqy(n)
      enddo
      call flush(8)
c
c      ..transport cmap params to isox11.c as we open the window...
c
                     call x11winope(
     *                %val(ixm)    ,%val(iym),%ref(maisox)
     *               ,%ref(isocols),%ref(irc)
     *               ,%val(phired), %val(phigreen), %val(phiblue)
     *               ,%val(  ared), %val(  agreen), %val(  ablue)
     *               ,%val(  pred), %val(  pgreen), %val(  pblue)
     *                             )
      write(*,102)maisox,isocols
c
c      set the colours for each process
c
      isocols=maisox
      do n=1,numtasks
         mycol(n)=(n*isocols)/(2*numtasks)
      enddo
c
      ipivotx=ixcentre-ipivot
      ipivoty=iycentre
      icantx=ixcentre+icant
      icanty=iycentre
      istart=1
      iend=npos
      id=1
   3  continue
c
c     stop
      call x11flush()
c     read(*,*)junk
      if(junk.lt.0)stop
      do n=istart,iend,id
c
         do m=2,npos
            call x11line(%val(ithet(m  )),%val(idldt(m  ))
     *                  ,%val(ithet(m-1)),%val(idldt(m-1)),%val(1))
            call x11line(%val(ithet(m  )),%val(iactr(m  ))
     *                  ,%val(ithet(m-1)),%val(iactr(m-1)),%val(1))
         enddo
c
         call x11line(%val(1),%val(iycentre)
     *               ,%val(ixm),%val(iycentre),%val(1))    ! X-axis
         call x11line(%val(ixcentre),%val(1)
     *               ,%val(ixcentre),%val(iym),%val(1))    ! Y-axis
c
         call x11line(%val(iqx(n)),%val(iqy(n))
     *               ,%val(ipx(n)),%val(ipy(n)),%val(1))   ! Line PQ
         call x11line(%val(ipx(n)),%val(ipy(n))
     *               ,%val(ipivotx),%val(ipivoty),%val(1)) ! Actuator
         call x11line(%val(ipx(n)),%val(ipy(n))
     *               ,%val(icantx),%val(icanty),%val(1))   ! Line PR
         call x11line(%val(ixcentre),%val(iycentre)
     *               ,%val(iqx(n)),%val(iqy(n)),%val(1))   ! Line OQ
         call x11flush() 
c        call nanopause(50000)
c
         call x11blackline(%val(iqx(n)),%val(iqy(n))
     *               ,%val(ipx(n)),%val(ipy(n)))
         call x11blackline(%val(ipx(n)),%val(ipy(n))
     *               ,%val(ipivotx),%val(ipivoty))
         call x11blackline(%val(ipx(n)),%val(ipy(n))
     *               ,%val(icantx),%val(icanty))
         call x11blackline(%val(ixcentre),%val(iycentre)
     *               ,%val(iqx(n)),%val(iqy(n)))
      enddo
c
      if(istart.eq.1
     *  )then
             istart=npos
             iend=1
             id=-1
         else
             istart=1
             iend=npos
             id=1
      endif
c
      go to 3
c
      read(*,*)junk
      go to 80
c
 99   continue
      write(0,200)
      stop
c
 80   continue
      stop
100   format(/)
101   format(12f9.2)
102   format(10i12)
c      
200   format('ERROR in cantilever.exe')

c      
      end
