      program test_butt
c
      use allcomms
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
      ixm=800
      iym=300
      allocate (zone_butt(0:ixm,0:iym))
c
      butt_height=20
      disp_top=button_line+butt_height+6
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
c      Open up the display window
c
      write(*,102)
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
             write(0,101)isocols,maxcols,irc
             stop
      endif
c
      acols=.01*float(maxcols-1)
      call button_setup('colstat')
  1   continue
c
      call x11spotbutton(nbut,mousex,mousey)
      if(mousey.lt.0)mousey=0
      if(mousex.lt.0)mousex=0
      if(nbut.ne.-999
     *  )then
             call locate_button
             write(*,104)nbut,mousex,mousey,kbutton
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
c      We are over a button - show a change
c
                    write(*,*)updown(kbutton)
                    n_pressed=kbutton
                    if(updown(kbutton).eq.1
     *                )then
                           call draw_butt(kbutton)
                       else
                           call draw_butt(kbutton)
                    endif
             endif
      endif
c

      if(nbut.eq.-994		! F3
     *  )then
             last_mousex=mousex
             iyrow1=mousey-idelta
             iyrow2=mousey+idelta
             call draw_ruler(iyrow1,iyrow2)
      endif
c
      if(nbut.eq.-703		! F3
     *  )then
              call x11mouse(nbut,kxcen,kycen,ixm,iym)
              write(*,105)nbut,kxcen,kycen,ixm,iym
c
c      If we get a second F3, then end the program
c
              if(nbut.eq.-703		! F3
     *          )then
                     write(*,*)'Program ending....'
                     stop
              endif
         else
             go to 1
      endif
c
      if(nbut.eq.-712
     *  )then
             stop
         else
             go to 1
      endif
c
100   format('TEST_BUTT: Cannot open X-window')
101   format('TEST_BUTT: isocols=',i3,', maxcols=',i3
     *      ,', Result code:',i8)
102   format('TEST_BUTT: About to open X-window')
104   format('TEST_BUTT: nbut=',i4,', mousex=',i6
     *                            ,', mousey=',i6,', button',i3)
105   format('TEST_BUTT: nbut=',i4,', kxcen=',i6,', kycen=',i6
     *      ,', ixm=',i6,', iym=',i6)
      end
