      subroutine dcv_wintxt(ncol)
c
c      Reads DCV status and puts text in X-window
c
      use allcomms
c
      parameter (lentxt=80)
      character (lentxt) text
c
      logical errfile
c
      lx=320
      lines=13
c
      lxa=lx-8
      nlines=lines
      call x11rectfi(%val(lxa),%val(0),%val(ixm),%val(iym),%val(ncol))
      call system('./DCV_summary')
      open(23,status='old')
  3   continue
         read(23,102,end=4,iostat=ios)text
         call x11text(%val(lx),%val(nlines),text, %val(0),%val(lentxt))
         nlines=nlines+13
         go to 3
  4   continue
      close(23)
c
c      Any errors?
c
             open(81, status='old',err=2)
             lya=nlines
             nlines=nlines+13
             mx=0
  1          continue
                read(81,102,end=5,err=2,iostat=ios)text
                mx=mx+1
                if(mx.eq.1
     *            )then
                       mcol=100.0*acols
                       call x11rectfi(%val(lxa),%val(lya),%val(ixm)
     *                               ,%val(iym),%val(mcol))
                endif
                call x11text(%val(lx),%val(nlines),text
     *                      ,%val(0),%val(lentxt))
                nlines=nlines+13
                go to 1
   5            continue
                close(81)
c               call system('cat ./fort.81')
  2          continue
             call system('rm -f fort.81')
c     endif
c
      call x11updatezone(%val(lxa),%val(0),%val(ixm),%val(iym))
c
      return
c
102   format(a)
c
      end
