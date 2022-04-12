      subroutine picker(nbut,kxcen,kycen,iwidth,iheight,kdy,dx,dy
     *                 ,simin,srmin)
c
c      Gets the results of pressing buttons, selecting new pic
c
      use isocomm
c
      real(8)xcen_box,ycen_box,delta_box,tres,deltay,dx,dy,simin,srmin
     *      ,t_cn, t_now,t_mouse
c
      logical resize,newposition
c
      character (len=200) selection_message
c           
      if(check)write(4,103)
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
      resize=.false.
  1   continue
      kd=0
      if(check
     *  )then  ! Irrelevant initialisations..
             mousex=ixm/2
             mousey=iym/2
             iwidth =0
             iheight=0
      endif
      kxold=kxcen
      kyold=kycen
      idxold=idx
      idyold=idy
      nbut=0
c
  2   continue
c
c      Issue a blocking call to check mouse or keyboard
c
      call x11mouse(nbut,mousex,mousey,iwidth,iheight)
      call tim(t_mouse)
      if(check
     *  )then
             write(*,101)t_mouse,nbut,mousex,mousey,iwidth,iheight
     *                                           ,ixm, iym,resize
             write(4,101)t_mouse,nbut,mousex,mousey,iwidth,iheight
     *                                           ,ixm, iym,resize
      endif
c
      if(nbut.eq.69)return  ! F3 pressed
      if(nbut.eq.36)nbut=1  ! Enter key proceeds to next picture
      if((nbut.gt.0.and.nbut.le.3)    !  For mouse input
     *   .or.
     *   (iwidth.eq.-1.and.(nbut.ge.10.and.nbut.le.12))  !  For keyboard input
     *  )then
c
c      Accept only button-releases for Man-Julia-Man changes...
c
             return
      endif
c
      if(resize
     *  )then
             call tim(t_now)
             if(nbut.eq.-10
c            if(t_now-t_cn.gt.1.0
     *         )then
                    nbut=111   !  New resize return code
                    resize=.false.
                    if(check
     *                )then
                           write(*,104)iwidth,iheight,t_now
                           write(4,104)iwidth,iheight,t_now
                    endif
                    return
             endif
      endif
c
      if(nbut.eq.-12
     *  )then
c
c      Window has received ConfigureNotify, so record details
c
             kxcen=kxold
             kycen=kyold
             if(.not.((ixm.eq.iwidth-1.and.iym.eq.iheight-1).or.
     *                (ixm.eq.iwidth  .and.iym.eq.iheight  ))
     *         )then
                    call tim(t_cn)
                    iwidthr =iwidth
                    iheightr=iheight
                    resize=.true.
                    if(check
     *                )then
                           write(*,105)iwidth,iheight,t_cn
                           write(4,105)iwidth,iheight,t_cn
                    endif
                    go to 2
             endif
      endif
c
      if(nbut.gt.-800
     *  )then
             kdy=2*idy
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
      if(iwidth.eq.0
     *  )then
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
             kxcen=mousex
             kycen=mousey
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
c     write(*,*)ixm,iym,deltay,kxcen,kycen,2*idy
c    *         ,xcen_box,ycen_box,delta_box
c
             write(selection_message,102)kxcen,kycen,2*idy,xcen_box
     *                                  ,ycen_box,delta_box
             call x11title(%ref(selection_message))
             kxcursor=kxcen
             kycursor=kycen
             newposition=.false.
      endif
      endif
c      
      go to 1
c
100   format('PICKER - Box resized to ',2i8,'. Centre at',2i8
     *      ,'. IDY=',i8)
111   format(/'Before X11M - NBUT:',i5
     *      ,', MouseX:',i5,', MouseY:',i5
     *      ,', Width:',i5,', Height:',i5,', IXM, IYM',2i5
     *      ,' RESIZE:',L2)
101   format(' After X11M: TIME:',f11.5,', NBUT:',i5
     *      ,', MouseX:',i5,', MouseY:',i5
     *      ,', Width:',i5,', Height:',i5,', IXM, IYM',2i5
     *      ,' RESIZE:',L2)
102   format('Next picture - Mouse button or keyboard ',
     *       'number: 1 Zoom in; 2 Mandelbrot<->Julia; 3 Back out.',
     *       ' F3 to end.',
     *  3i6,2e26.17,e12.4)
103   format(/'***********************'
     *      ,/'*** Entering PICKER ***'
     *      ,/'***********************',/) 
104   format('Returning to resize:',2i5,' at t=',f11.5,/)
105   format('   Resizing capture:',2i5,' at t=',f11.5)
      end
