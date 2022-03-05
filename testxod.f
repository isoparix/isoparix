      program testxod
c
c     Tests XOpenDisplay call
c
      use bmp_comms
c
      call defmap(254,7,iret)    ! Sets colour map
c
      ixm=800
      iym=600
      maisox=200
      write(*,102)maisox,isocols,irc
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
      if(irc.ne.0
     *  )then
             write(0,100)irc
             stop
      endif
c
      ix1=ixm/10
      iy1=iym/10
      ix2=ixm/2
      iy2=iym/2
      ncol=100
      write(*,*)ix1,iy1,ix2,iy2,ncol
      call x11rectgc(%val(ix1),%val(iy1)
     *              ,%val(ix2),%val(iy2),%val(ncol))
      call x11flush()
      call microsleep(1000000)
      call x11close()
c
      stop
c
100   format('Stopping with return code:', i6)      
102   format('MAISOX=',i8,', ISOCOLS=',i8,', RESULT=',i8)
      end
