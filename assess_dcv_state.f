      subroutine assess_dcv_state
c
      use allcomms
      use dcv_comms
c
c      Have there been any changes?
c
      state_change=.false.
c
      if(rvn.neqv.rvn_old
     *  )then
             rvn_old=rvn
             state_change=.true.
      endif
c
      if(svn.neqv.svn_old
     *  )then
             svn_old=svn
             state_change=.true.
      endif
c
      if(wall_file.ne.wall_file_old
     *  )then
             wall_file_old=wall_file
             state_change=.true.
      endif
c
      if(transport.ne.transport_old
     *  )then
             transport_old=transport
             state_change=.true.
      endif
c
      if(mpicomm.ne.mpicomm_old
     *  )then
             mpicomm_old=mpicomm
             state_change=.true.
      endif
c
      if(nsync.ne.nsync_old
     *  )then
             nsync_old=nsync
             state_change=.true.
      endif
c
      if(disp_pointer.neqv.disp_pointer_old
     *  )then
             disp_pointer_old=disp_pointer
             state_change=.true.
      endif
c
      if(window_selector.neqv.window_selector_old
     *  )then
             window_selector_old=window_selector
             state_change=.true.
      endif
c
      if(client_render.neqv.client_render_old
     *  )then
             client_render_old=client_render
             state_change=.true.
      endif
c
      if(local_address.ne.local_address_old
     *  )then
             local_address_old=local_address
             state_change=.true.
      endif
c
      if(frame_sync.neqv.frame_sync_old
     *  )then
             frame_sync_old=frame_sync
             state_change=.true.
      endif
c
      if(state_change
     *  )then
             do kbutton=1,ntop_button
                if(exist_button(kbutton)
     *            )then
                       call dcv_butt_action
                endif
             enddo
c
             write(*,103)rvn,svn
             if(.not.svn
     *         )then
                    return
             endif
             write(*,1031)wall_file,transport
c            if(transport.eq.'MPI'
c    *         )then
c                   write(*,1032)mpicomm
c            endif
c     write(*,*)nsync,nsync_old
             write(*,1033)nsync,disp_pointer,window_selector
     *                  ,client_render,frame_sync
      endif
c
      return
c
103   format('    RVN state is',l2
     *     ,/'    SVN state is',l2)
1031  format('    Wall file is ',a
     *     ,/'    Transport is ',a)
1032  format('      mpicomm is ',a)
1033  format('         Sync is',i2
     *     ,/'      Pointer is',l2
     *     ,/'Window select is',l2
     *     ,/'Client render is',l2
     *     ,/'   Frame sync is',l2
     *     ,//)
c
      end
