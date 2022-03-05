      module dcv_comms
c
      logical rvn,svn,disp_pointer
     *       ,window_selector,client_render,frame_sync
     *       ,rvn_old,svn_old,disp_pointer_old
     *       ,window_selector_old,client_render_old,frame_sync_old
     *       ,state_change
c
      character (60) wall_file    ,transport    ,local_address    
     *              ,mpicomm
      character (60) wall_file_old,transport_old,local_address_old
     *              ,mpicomm_old
c
      integer nsync,nsync_old
c
      end
