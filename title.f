      subroutine title
c
c      Displays information in X-window title bar...
c
      use isocomm
c
      if(.not.graphics)return
c
      infomsg=' ISOPARIX - Version 1.05: '//txtout//'\0'
      call x11title(%ref(infomsg))
      call tim(ttitle)
      author=.false.
c
      return
      end
