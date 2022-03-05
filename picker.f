      subroutine picker(nbut,kxcen,kycen,kdy)
c
c      Gets the results of pressing buttons, selecting new pic
c
      use isocomm
c
      logical resize
c
c      Initialise box on screen
c
      resize=.false.
      idy=.5+(.05*dfloat(iym))
      idx=.5+(dfloat(idy)*dfloat(ixm)/dfloat(iym))
      kxcen=ixm/2
      kycen=iym/2
      call x11box(%val(kxcen-idx),%val(kycen-idy)
     *           ,%val(kxcen+idx),%val(kycen+idy))
  1   continue
      kd=0
      kxold=kxcen
      kyold=kycen
      idxold=idx
      idyold=idy
c
  2   continue
c
c      Issue a blocking call to check mouse or keyboard
c
      call x11mouse(nbut,kxcursor,kycursor,ixresize,iyresize)
      if(nbut.ne.-994)write(0,101)nbut
      if(nbut.eq.69)return  ! F3 pressed
      if(nbut.gt.0.and.nbut.le.3    !  For mouse input
     *   .or.
     *   nbut.ge.10.and.nbut.le.12  !  For keyboard input
     *  )then
c
c      Accept only button-releases...
c
             return
      endif
c
      if(nbut.eq.-10
     *  )then
c
c      Window has been exposed, so refresh it..
c
             go to 2
      endif
c
      if(nbut.eq.-11
     *  )then
c
c      Window has been resized, so record details
c
             ixm=ixresize
             iym=iyresize
             resize=.true.
             go to 2
      endif
c
      if(nbut.gt.-800
     *  )then
             kdy=2*idy
             if(resize
     *         )then
                    nbut=-11
                    kxcen=kxcursor
                    kycen=kycursor
c                   write(*,100)ixm,iym,kxcen,kycen,idy
             endif
c            return
      endif
c
      kd=0
c
c      ***** Box size change *****
c
      if(nbut.eq.60)kd= 1   ! >
      if(nbut.eq.59)kd=-1   ! <
c
c      Enlarge or shrink box
c
      idy=idy+kd
      if(idy.lt.2)idy=2
      idx=.5+(real(idy*ixm)/real(iym))
c
c      ***** Box translation *****
c
      if(nbut.eq.104.or.nbut.eq.88)kxcen=kxcen-1  !   Down arrows
      if(nbut.eq. 98.or.nbut.eq.80)kxcen=kxcen+1  !   Up
      if(nbut.eq.100.or.nbut.eq.83)kycen=kycen-1  !   Left
      if(nbut.eq.102.or.nbut.eq.85)kycen=kycen+1  !   Right
      if(nbut.eq.-994
     *  )then
             kxcen=kxcursor
             kycen=kycursor
      endif
c
c      Wipe out old box...
c
      call x11box(%val(kxold-idxold),%val(kyold-idyold)
     *           ,%val(kxold+idxold),%val(kyold+idyold))
      call x11flush()
c
c      ...and draw the new one
c
      call x11box(%val(kxcen-idx),%val(kycen-idy)
     *           ,%val(kxcen+idx),%val(kycen+idy))
c
      go to 1
c
100   format('PICKER - Resized to ',2i8,'. Centre at',2i8,'. IDY=',i8)
101   format('PICKER - button pressed was ',i5)
c
      end
