      program iostatmon
c
c      Reads iostat records from standard input, puts out single lines
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
!Error Unknown operating systen
!#endif
      source_id=hostid//'                            '
c
c      How many disks do we have?
c
      call io_sizer
c
      nqsamp      =0
      maxdevs     =0
      maxhosts    =1
      maxtimes    =0
      maxrows     =0
      max_screen_x=0
      max_screen_y=0
c
      call mem_alloc('iostat  ')
c
      write(8,100)trim(hostid),maxdev_s
c
  1   continue
      call convrec(source_id,.true.)
      if(end_data)stop
      go to 1
c
100   format(a,' has',i4,' disks')
c
      end
