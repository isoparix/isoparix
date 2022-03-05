      subroutine select_view
c
c      What do we display?
c
      use allcomms
c
      nrow_ruler_old=-1
c
      if(ndisp_type.eq.1
     *  )then
             allhosts=.true.
             call win_type('ALL   ')
      endif
c
      if(ndisp_type.eq.2
     *  )then
             allhosts=.false.
             call win_type('CPU   ')
      endif
c
      if(ndisp_type.eq.3
     *  )then
             allhosts=.false.
             ndisk_view=1
             delta_disk_view=.true.
             call win_type('CPUDSK')
      endif
c
      do n=1,nrows
         inverted_name(n)=.false.
      enddo
c
      return
      end
