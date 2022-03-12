      subroutine picker(nbut,kxcen,kycen,kdy,dx,dy,simin,srmin)
c
c      Gets the results of pressing buttons, selecting new pic
c
      use isocomm
c
      real(8)xcen_box,ycen_box,delta_box,tres,deltay,dx,dy,simin,srmin
c
      logical resize,newposition
c
      character (len=200) selection_message
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
c     if(nbut.ne.-994.and.check)write(0,101)nbut
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
      if(nbut.eq.-10
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
                    nbut=-10
                    kxcen=kxcursor
                    kycen=kycursor
                    write(0,100)ixm,iym,kxcen,kycen,idy
             endif
c            return
      endif
c
      kd=0
      newposition=.false.
c
c      ***** Box size change *****
c
      if(nbut.eq.60)kd= 1   ! >
      if(nbut.eq.59)kd=-1   ! <
      if(kd.ne.0)newposition=.true.
c
c      Enlarge or shrink box
c
      idy=idy+kd
      if(idy.lt.2)idy=2
      idx=.5+(real(idy*ixm)/real(iym))

c
c      ***** Box translation *****
c
      if(nbut.eq.104.or.nbut.eq.88
     *  )then
             newposition=.true.
             kycen=kycen+1  !   Down arrows
      endif
c
      if(nbut.eq. 98.or.nbut.eq.80
     *  )then
             newposition=.true.
             kycen=kycen-1  !   Up
      endif
c
      if(nbut.eq.100.or.nbut.eq.83
     *  )then
             newposition=.true.
             kxcen=kxcen-1  !   Left
      endif
c
      if(nbut.eq.102.or.nbut.eq.85
     *  )then
             newposition=.true.
             kxcen=kxcen+1  !   Right
      endif
c
      if(nbut.eq.-994
     *  )then
             newposition=.true.
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
      if(newposition
     *  )then
             xcen_box=srmin+(dx*dfloat(kxcen))
             ycen_box=simin+(dy*dfloat(iym-kycen))
             delta_box=dy*dfloat(2*idy)
c     write(0,*)ixm,iym,deltay,kxcen,kycen,2*idy
c    *         ,xcen_box,ycen_box,delta_box
c
             write(selection_message,102)kxcen,kycen,2*idy,xcen_box
     *                                  ,ycen_box,delta_box
             call x11title(%ref(selection_message))
             kxcursor=kxcen
             kycursor=kycen
             newposition=.false.
      endif
c      
      go to 1
c
100   format('PICKER - Resized to ',2i8,'. Centre at',2i8,'. IDY=',i8)
101   format('PICKER - button pressed was ',i5)
102   format('Next picture - Mouse button or keyboard ',
     *       'number: 1 Zoom in; 2 Mandelbrot<->Julia; 3 Back out.',
     *       ' F3 to end.',
     *  3i6,2e26.17,e12.4)
      end
