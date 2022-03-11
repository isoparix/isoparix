      program topmon
c
c      Reads top records from standard input, puts out single lines
c
      use allcomms
c
      character (32) hostid
      character (60) source_id
c
      nchann=5
c
!#ifdef cygwin
      ishost=hostnm(hostid)
!#elif defined _AIX
!      ishost=hostnm_(hostid)
!#else
!Error Unknown operating system
!#endif
      source_id=hostid//'                            '
c
c      How many cpus do we have?
c
      nqsamp      =0
      maxdevs     =0
      maxhosts    =1
      maxtimes    =0
      maxrows     =0
      max_screen_x=0
      max_screen_y=0
c
  1   continue
      call toprec(source_id,.true.)
      if(end_data
     *  )then
             stop
         else
             go to 1
      endif
c
      end
