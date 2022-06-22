      program showcubes
c
c      Projects rotating cube on to a plane
c
      use isocomm
      use bmp_comms
c
      implicit real(8) (a-h,o-z)
c
      logical movie 
c
      check=.false.
      lchann=10
c
c      Read in the details of the colour map
c
      call defmap(254,7,iret)
      if(iret.ne.0
     *  )then
             stop
      endif       
c
      call tripleset
      call cubeinit
      numtasks=8
      movie=.true.
      write(*,103)
c     read(*,*)ixm,iym,ndelay
      ixm=600
      iym=400
      ndelay=100000
c
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
c
c      Check the return code from x11winope...
c
      if(irc.lt.0   !  Failure
     *  )then
             write(0,107)irc,ixm,iym,maisox,isocols
     *               ,phired, phigreen, phiblue
     *               ,  ared,   agreen,   ablue
     *               ,  pred,   pgreen,   pblue
             stop
      endif
c
c      set the colours for each process
c
      if(isocols.eq.0
     *  )then
             isocols=maisox-1
      endif
c
      do n=1,numtasks
         mycol(n)=(n*isocols)/(2*numtasks)
         write(*,105)n,mycol(n),isocols
      enddo
      nedgecol=mycol(1)
c
      theta=30
      phi=60
      phi_delta=.5
      theta_delta=phi_delta*sqrt(2.)
      call screenscale(int(theta),int(phi))
c
      diag_delta=.01*diagonal
c
      call cubeadd(1,   ixm/8,-iym/2,    ixm/2, -iym/8,10)
      call cubeadd(2,  ixm/8,  iym/8,    ixm/2,  iym/2,40)
      call cubeadd(3, -ixm/2, -iym/2,   -ixm/3, -iym/3,70)
      call cubeadd(4, -ixm/2,  iym/4,-7*ixm/16,  iym/3,20)
      call cubeadd(5,-ixm/10,      1,        0, iym/10,20)
      call cubeadd(6,-ixm/10,-iym/10,        0,      0,40)
      call cubeadd(7,      1,-iym/10,   ixm/10,      0,60)
      call cubeadd(8,      1,      1,   ixm/10, iym/10,80)
c
c     ibatch=100
      ibatch=100
      abatch=ibatch
      npass=0
  1   continue
c        read(*,*,end=2)    n,ix1,iy1,ix2,iy2,ih
c        write(*,102)n,ix1,iy1,ix2,iy2,ih
c        call cubeadd(n,ix1,iy1,ix2,iy2,ih)
c
         call tim(told)
  2      continue
         do n=1,ibatch
             if(movie
     *         )then
                    call nanopause(ndelay)
c                   call x11sync()
                    theta=theta+theta_delta
c                   phi=phi+phi_delta
                    nbut=0
                else
                    call x11mouse(nbut,kxcen,kycen,ixm,iym)
c
                    if(nbut.eq.-800)theta=theta-theta_delta
                    if(nbut.eq.-802)theta=theta+theta_delta
c
                    if(nbut.eq.-801)phi=phi-phi_delta
                    if(nbut.eq.-803)phi=phi+phi_delta
c
                    if(nbut.eq.-995)diagonal=diagonal+diag_delta
                    if(nbut.eq.-996)diagonal=diagonal-diag_delta
c
                    if(nbut.le.-500.and.nbut.ge.-509
     *                )then
                           nc=-(nbut+500)
                           if(nc.gt.0.and.nc.le.ncubes
     *                       )then
                                  call cubecheck(nc)
                           endif
                    endif
c
                    if(nbut.eq.-703)stop
             endif
c
             if(nbut.ne.-994
     *         )then
c
c      Not a MotionNotify...
c
                    call eyepoint(diagonal,theta,phi)
                    call x11clearpixmap() 
                    call scene
                    call axes
                    call x11flush()
              endif
         enddo
         if(check)stop
         call tim(tnew)
         fps=abatch/(tnew-told)
         told=tnew
         write(*,101)fps
         npass=npass+1
         if(npass.gt.100)stop
      go to 1
c
100   format('NBUT:',i5)
101   format(f6.1,' frames per second')
102   format(10i8)
103   format('Integer width, height, delay_in_micro-seconds?')
105   format('Colour for task',i2,' is',i4,' of',i4)
106   format('SHOWCUBES:     R=',f8.1,', THETA=',f8.1,', PHI=',f8.1)
107   format('SHOWCUBES:  Return code is',i4
     *      ,'            ixm=',i5,', iym=,',i5
     *      ,', maisox=',i5,', isocols=',i5
     *     ,/3('         ',3f10.3,/)
     *      )
      stop
      end
