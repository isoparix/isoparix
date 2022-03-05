      subroutine buttons(blocking)
c
      use allcomms
c
      character (len=55) blanks
      character (len=20) text
c
      logical blocking,old_win
c
      blanks='                                                       '
      end_prog=.false.
c
      nbut=0
      mousex=0
      mousey=0
      idummx=0
      idummy=0
c
      if(blocking
     *  )then
             call x11mouse(nbut,mousex,mousey,idummx,idummy)
         else
             call x11spotbutton(nbut,mousex,mousey)
      endif
c
c      Were any arrow keys pressed?
c
      call arrows(nbut)
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
              return
      endif
c
c######################################################################
c#                                                                    #
c#             NBUT=-994                                              #
c#                                                                    #
c######################################################################
c
      if(nbut.eq. -994
     *  )then
c
c      Pointer has been moved...
c
c     write(*,300)nrow_ruler,lastrow,mousex,mousey,nendcol
             if(mousey.ge.disp_top.and.
     *          mousey.lt.disp_bot
     *         )then
                    nrow_ruler=line_host(mousey)
c     write(*,300)nrow_ruler,lastrow,mousex,mousey,nendcol
                    if(ndisp_type.eq.1)nh_select=line_host(mousey)-7
                    if(nh_select.lt.1)nh_select=1
                    job_node=nodename(nh_select)
                    ssh_node=ssh_name(nh_select)
                     if(mousex.gt.nhsp          .and.
     *                  mousex.le.nendcol+nhsp
     *                 )then
c
c      First wipe out any existing ruler, then draw a new one
c
                            last_mousex=mousex
c
                            call draw_ruler(line_y(nrow_ruler)
     *                                     ,line_y(nrow_ruler+1))
                     endif
             endif
c
c      Display data rates corresponding to new position
c
              call title_rates
c
c             write(*,108)zone_butt(mousex,mousey),mousex,mousey
              call locate_button
              return
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
c      We are over a 'statview' button - show a change
c
                    write(*,111)kbutton
                    n_pressed=kbutton
                    updown(kbutton)=-1
                    call draw_butt(kbutton)
                    return
             endif
c
      endif
c
c######################################################################
c#                                                                    #
c#             NBUT=1                                                 #
c#                                                                    #
c######################################################################
c
      if(nbut.eq.1
     *  )then
c
c      Mouse button 1 has been released...
c
             if(over_button
     *         )then
c
c      We are over a 'statview' button
c
                    updown(kbutton)=1 
                    call draw_butt(kbutton)
                    write(*,110)kbutton,n_pressed
                    if(n_pressed.eq.kbutton
     *                )then
c
c      The button over which we are releasing is the same as the 
c      one we pressed....
c
                           call button_action(kbutton)
                           n_pressed=0
                    endif
c
                    call draw_butt(kbutton)
                    return
             endif
c
             if(mousey.ge.disp_top.and.
     *          mousey.lt.disp_bot
     *         )then
                    if(ndisp_type.eq.1
     *                )then
                           nh_select=line_host(mousey)-7
c
                           if(nh_select.lt.0
     *                       )then
c
c      We're selecting a new mode for all hosts (usr/sys/wait/idle)
c
                                  global_type=line_host(mousey)
c                   write(*,103)nbut,mousex,mousey,nh_select,global_type
                                  old_win=new_win
                                  new_win=.true.
                                  call column_set
                                  call paint_hosts
                                  new_win=old_win
                                  call column_set
                                  return
                           endif
c
                           if(nh_select.gt.0
     *                       )then
c
c      We're selecting a single host to view
c
                                  ndisp_type=2
                                  ncore_view=1
                                  call select_view
                                  call paint_all
                                  return
                           endif
c
                           return
c
                   endif     ! End of ndisp_type.eq.1
c
                   if(ndisp_type.eq.2
     *               )then
c
c      Deal with CPU lines
c
                           if(line_host(mousey).eq.1
     *                       )then
c
c      Move on to next display mode (disks)
c
                                  ndisp_type=3
                                  ndisk_view=1
                                  call select_view
                                  call paint_all
                                  return
                           endif
c
c      This is one of usr/sys/idle/wait - display for all cores
c
                           if(line_host(mousey).ge.3.and.
     *                        line_host(mousey).le.6
     *                       )then
                                  ncore_view=line_host(mousey)-2
                                  call select_view
                                  call paint_all
                                  return
                           endif
c
                           return
                    endif
c
c
                    if(line_host(mousey).eq.1
     *                )then
                           if(ndisp_type.eq.3
     *                       )then
c
c      Move back to global view
c
                                  ndisp_type=1
                                  call select_view
                                  call paint_all
                                  return
                           endif
                    endif
c
                    if(line_host(mousey).eq.7
     *                 )then
c
                           if(ndisp_type.eq.3
     *                       )then
c
c      Change view of disks
c
                                  ndisk_view=ndisk_view+1
                                  ndisk_view=mod(ndisk_view
     *                                          ,ndisk_view_max)
                                  if(redhat.and.ndisk_view.eq.0
     *                              )then
c
c      Redhat can't do 'percentage active'
c
                                         write(*,113)aix,redhat
                                         ndisk_view=1
                                  endif
c                   write(*,1031)nbut,mousex,mousey,nh_select,ndisk_view
                                  delta_disk_view=.true.
                                  call paint_all
                                  return
                           endif
c
                           return
c
c                                 if(.not.host_track(nh_select)
c    *                              )then
c                                        write(0,102)nh_select
c    *                                ,trim(nodename(nh_select))
c                                        return
c                                 endif
c
                    endif
             endif
      endif
c
c      Handle F-key presses...
c
c######################################################################
c#                                                                    #
c#             NBUT=-701 to -712                                      #
c#                                                                    #
c######################################################################
c
      if(nbut.le.-701.and.nbut.ge.-712
     *  )then
             kbutton=-700-nbut
             write(0,112)kbutton
             updown(kbutton)=-1
             call draw_butt(kbutton)
             call x11flush()
             call button_action(kbutton)
             call system('sleep 1')
             updown(kbutton)=1
             call draw_butt(kbutton)
             return
      endif
c
      return
c
101   format('Button',i2,' released at',2i4,' - Host ID:',i3)
102   format('BUTTONS: Host ',i4,'(',a,') has not yet supplied data')
103   format('Button',i2,' released at',2i4,' - Host ID:',i3
     *      ,' Global_type:',i3)
1031  format('Button',i2,' released at',2i4,' - Host ID:',i3
     *      ,' Disk view:',i3)
104   format('BUTTONS: nbut=',i4,', mousex=',i6,', mousey=',i6
     *      ,', blocking=',l1)
108   format('zone_butt=',i3,' at (',i5,',',i5,')')
110   format('Button',i3,' released.  Button pressed was',i3)
111   format('Button',i3,' pressed',i3)
112   format('F-key',i2.2,' pressed')
113   format('BUTTONS: AIX=',l1,', REDHAT=',l1,', NDISK_VIEW changed'
     *      ,' from zero to one.')
300   format('NROW_RULER=',i4,', LASTROW=',i4
     *        ,', MOUSEX=',i5,', MOUSEY=',i5,', NENDCOL=',i5)
c
      end
