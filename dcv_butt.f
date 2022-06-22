      program dcv_butt
c
      use allcomms
      use dcv_comms
c
      character(15) text
c
      real(8)phired,phigreen,phiblue
     *        ,ared,  agreen,  ablue
     *        ,pred,  pgreen,  pblue
c
c      Read in the details of the colour map
c
      phired  =0.0
      phigreen=0.0
      phiblue =0.5
c
      ared  =0.0
      agreen=0.4
      ablue =0.0
c
      pred=  1.0
      pgreen=4.0
      pblue= 1.0
c
      pi_math=355./113.
      phired=  phired  *pi_math
      phigreen=phigreen*pi_math
      phiblue= phiblue *pi_math
c
      ixm=1000
      iym=300
      allocate (zone_butt(0:ixm,0:iym))
c
c
c      Buttons....
c
      ihsepbutt=2
      ivsepbutt=30
      disp_top=2*ivsepbutt
      leftbutt=20
      butt_height=20
      zone_butt=-1
c
      idelta=6
      disp_top=0
      disp_bot=iym
      ndelta_x=idelta
      ndy1=0
      ndy2=0
      lmxold=0
c
      text=' Processing... '
      ixt=180
      iyt=50
      ixta=ixt+(15*8)
      iyta=iyt+13
      iytb=iyt+18
c
c      Open up the display window
c
      call x11winope
     * (%val(-ixm)
     * ,%val(iym)
     *             ,%ref(maxcols),%ref(isocols),%ref(irc)
     *             ,%val(phired), %val(phigreen), %val(phiblue)
     *             ,%val(  ared), %val(  agreen), %val(  ablue)
     *             ,%val(  pred), %val(  pgreen), %val(  pblue)
     *               )
c
c      Window OK?
c
      if(irc.lt.0
     *  )then
             write(0,100)
             stop
      endif
c
c     write(0,101)isocols,maxcols
      acols=.01*float(maxcols-1)
c
      infomsg='                  DCV_Panel - J S Watts IBM UK Ltd 2007'
      call x11title(infomsg)
c
  2   continue
c
c      Initialise buttons by colouring background and reading current 
c      DCV state
c
      call read_dcv_state
c     write(*,107)
c
      ncol   =10.0*acols
      ncoltxt= 2.0*acols
      call x11rectfi(%val(0),%val(0),%val(ixm),%val(iym),%val(ncol))
c
      call button_setup('dcvbutt')
      call dcv_wintxt(ncoltxt)
      call x11flush()
c
  1   continue
c
      call x11mouse(nbut,mousex,mousey,idummx,idummy)
c     write(*,*)nbut,mousex,mousey
c
c######################################################################
c#                                                                    #
c#             NBUT=-10                                               #
c#                                                                    #
c######################################################################
c
      if(nbut.eq.-10
     *  )then
c
c      Window has been exposed - redraw it...
c
              call x11updatezone(%val(0),%val(0),%val(ixm),%val(iym))
              call x11flush()
      endif
c
      if(nbut.ne.-999
     *  )then
             call locate_button
c            write(*,104)nbut,mousex,mousey,kbutton
      endif
c
c######################################################################
c#                                                                    #
c#             NBUT=-1                                                #
c#                                                                    #
c######################################################################
c
      if(nbut.eq.-1
     *  )then
c
c      Mouse button 1 has been pressed....
c
             if(over_button
     *         )then
c
c      Do its action
c
c#######################
                     mcol=100.0*acols
                     call x11rectfi(%val(ixt),%val(iyt),%val(ixta)
     *                                      ,%val(iytb),%val(mcol))
                     call x11text(%val(ixt),%val(iyta),text
     *                             ,%val(0),%val(15))
                     call x11updatezone(%val(ixt), %val(iyt)
     *                                 ,%val(ixta),%val(iytb))
c#######################
                     mx=updown(kbutton)
                     call dcv_butt_action
c
                     if(kbutton.eq.13
     *                 )then
c
c      The exit button
c
                            updown(kbutton)=-1
                            call draw_butt(kbutton)
                            call nanopause(300000)
                            stop
                     endif
c
                     if(kbutton.eq.15
     *                 )then
c
c      The Help button
c
                            updown(kbutton)=-1
                            call draw_butt(kbutton)
                            call dcv_activate(kbutton)
                            call nanopause(300000)
                            updown(kbutton)=1
                            call draw_butt(kbutton)
                            go to 3
                     endif
c
                     if(mx.ne.updown(kbutton)
     *                 )then
c
c      The button state has been altered, so do the action
c
                            call dcv_activate(kbutton)
                            if(kbutton.ge.2.and.
     *                         updown(2).lt.0
     *                        )then
                       call system('./svn_command >fort.81 2>/dev/null')
                            endif
c
c      Read DCV status and put text in X-window
c
                            call dcv_wintxt(ncoltxt)
                            call read_dcv_state
                     endif
c
  3                  continue
c
                     call x11rectfi(%val(ixt),%val(iyt)
     *                             ,%val(ixta),%val(iytb),%val(ncol))
                     call x11updatezone(%val(ixt), %val(iyt)
     *                                 ,%val(ixta),%val(iytb))
c
                     if(kbutton.eq.2.and.
     *                  updown(kbutton).eq.-1
     *                 )then
                            go to 2
                     endif
             endif
      endif
c
      if(nbut.eq.-994		! F3
     *  )then
             last_mousex=mousex
             iyrow1=mousey-idelta
             iyrow2=mousey+idelta
      endif
c
c     call nanopause(100000)
c     call read_dcv_state
c     call assess_dcv_state
c     if(state_change
c    *  )then
c            go to 2
c        else
             go to 1
c     endif
c
100   format('DCV_BUTT: Cannot open X-window')
101   format('DCV_BUTT: isocols=',i3,', maxcols=',i3)
104   format('DCV_BUTT: nbut=',i4,', mousex=',i6,', mousey=',i6
     *      ,', Button:',i3)
105   format('DCV_BUTT: nbut=',i4,', kxcen=',i6,', kycen=',i6
     *      ,', ixm=',i6,', iym=',i6)
107   format('DCV_BUTT: Painting initial window')
      end
