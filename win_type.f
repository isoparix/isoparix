      subroutine win_type(style)
c
c      Opens a window for all-host stats or single-host stats
c
      use allcomms
c
      character (len=6) style
      character (len=60) host
c
      new_win=.true.
      fullmap=.true.
      nrow_ruler=1
c
      if(open_window
     *  )then 
             write(*,104)
      endif
c
      if(style.eq.'ALL   '
     *  )then 
             write(*,101)
             nrows=nhosts+n_cpu_attributes+5
             call start_win
             infomsg=title_header//
     *               '   CPU usage from all sources \0'
             call x11title(infomsg)
             return
      endif
c
      if(style.eq.'CPUDSK'
     *  )then
             write(*,102)
             host=trim(nodename(nh_select))
             nrows=n_cpu_attributes+5+ndisks(nh_select)
             call start_win
             infomsg=title_header//
     *               ' Detailed CPU/disk of source ' //host//'\0'
             call x11title(infomsg)
             return
      endif
c
      if(style.eq.'CPU   '
     *  )then
             write(*,103)
             host=trim(nodename(nh_select))
             nrows=n_cpu_attributes+5+ncores(nh_select)
             call start_win
             infomsg=title_header//
     *               ' Detailed CPU of source ' //host//'\0'
             call x11title(infomsg)
             return
      endif
c
      write(0,100)style
      return
c
100   format('WIN_TYPE: Unrecognised style: ',a6,' - no change')
101   format('          CPU usage from all sources')
102   format('          detailed CPU/disk of source')
103   format('          detailed CPU of source')
104   format('WIN_TYPE: Resizing X-window to change to view')
c
      end
