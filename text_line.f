      subroutine text_line(mrow)
c
c      Displays a line of text in row MROW starting just after
c      left-hand titles...
c
      use allcomms
c
      iy1=line_y(mrow)+1
      iy2=line_y(mrow+1)-1
      iytxt=((iy1+iy2)/2)+4
      lentext=len_trim(title)
c
c      Blank out line first...
c
      call x11rectgc(%val(0)    ,%val(iy1)
     *              ,%val(ixm-1),%val(iy2),%val(nblack))
      call x11text(%val(nhsp+1),%val(iytxt),title,%val(0),%val(lentext))
      call x11updatezone(%val(   0) ,%val(iy1)
     *                   ,%val(ixm-1),%val(iy2))
c
      return
      end
