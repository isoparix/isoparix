      subroutine da_open(filename,nsize,mode,topen)
c
c      Opens the file of record length nsize in mode
c
      character (50) filename
      character (6) mode
      character (7) stat
c
      real (8) topen,tb,tdelta
c
      if(mode.eq.'write '
     *  )then
             stat='unknown'
      endif
c
      if(mode.eq.'read  '
     *  )then
             stat='old    '
      endif
c
      if(mode.eq.'update'
     *  )then
             stat='old    '
             mode='write '
      endif
c
      call tim(topen)
      open(3
     *    ,file=filename
     *    ,access='direct'
     *    ,form='unformatted'
     *    ,recl=nsize
     *    ,action=mode
     *    ,status=stat
     *    ,err=99
     *    )
      call tim(tb)
      topen=tdelta(tb,topen)
c     write(*,101)trim(filename),topen,mode
c
      return
c
 99   continue
      write(0,100)trim(filename),mode
      stop
c
100   format('***** ERROR in DA_OPEN:  Could not open ',a
     *      ,' in mode ',a)
101   format(/'File ',a,' opened in',f10.4,' seconds in mode ',a)
c
      end
