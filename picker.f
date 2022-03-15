      subroutine picker(nbut,kxcen,kycen,ixmp,iymp,kdy,dx,dy
     *                 ,simin,srmin)
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
      idy=.5+(.05*dfloat(iym))
      idx=.5+(dfloat(idy)*dfloat(ixm)/dfloat(iym))
      kxcen=ixm/2
      kycen=iym/2
      call x11box(%val(kxcen-idx),%val(kycen-idy)
     *           ,%val(kxcen+idx),%val(kycen+idy))
      nresize=0
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
      call x11mouse(nbut,kxcorner,kycorner,ixmp,iymp)
      if(nbut.ne.-994.and.nbut.ne.-999.and.check)write(0,101)
     *nbut,kxcorner,kycorner,ixmp,iymp,nresize
      if(nbut.eq.69)return  ! F3 pressed
      if(nbut.eq.36)nbut=1  ! Enter key proceeds to next picture
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
c     if(nbut.eq.-10
c    *  )then
c
c      Window has been exposed, so refresh it..
c
c            go to 2
c     endif
c
      if(nbut.eq.-12
     *  )then
c
c      Window has been resized, so record details
c
             kxcen=kxold
             kycen=kyold
             if(ixm.ne.ixmp-1.or.iym.ne.iymp-1)return
      endif
c
      if(nbut.gt.-800
     *  )then
             kdy=2*idy
c            if(resize
c    *         )then
c                   nbut=-10
c                   kxcen=kxcorner
c                   kycen=kycorner
c                   write(0,100)ixm,iym,kxcen,kycen,idy
c            endif
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
             kxcen=kxcorner
             kycen=kycorner
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
100   format('PICKER - Box resized to ',2i8,'. Centre at',2i8
     *      ,'. IDY=',i8)
101   format('PICKER - NBUT:',i5
     *      ,', KXCORNER:',i5,', KYCORNER:',i5
     *      ,', IXMP:',i5,', IYMP:',i5,', NRESIZE:',i4)
102   format('Next picture - Mouse button or keyboard ',
     *       'number: 1 Zoom in; 2 Mandelbrot<->Julia; 3 Back out.',
     *       ' F3 to end.',
     *  3i6,2e26.17,e12.4)
      end
