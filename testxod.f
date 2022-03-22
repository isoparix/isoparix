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
      ncol=120
      write(*,*)ix1,iy1,ix2,iy2,ncol
      call x11rectgc(%val(ix1),%val(iy1)
     *              ,%val(ix2),%val(iy2),%val(ncol))
      call x11flush()
      write(*,*)'F3 to end'
c
c      Issue a blocking call to check mouse or keyboard
c
  1   continue
      call x11mouse(nbut,kxcorner,kycorner,ixmp,iymp)
      write(0,101)nbut,kxcorner,kycorner,ixmp,iymp,ixm,iym
      if(nbut.eq.69
     *  )then
             call x11close()
             stop
         else
             go to 1
      endif
c
100   format('Stopping with return code:', i6)      
101   format('PICKER - NBUT:',i5
     *      ,', KXCORNER:',i5,', KYCORNER:',i5
     *      ,', IXMP:',i5,', IYMP:',i5,', IXM, IYM',2i5)
102   format('MAISOX=',i8,', ISOCOLS=',i8,', RESULT=',i8)
      end
