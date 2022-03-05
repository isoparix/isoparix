      subroutine locate_button
c
c      Spots whether we are over a button, or if we WERE over a
c      button.    If we are newly over a button then we note it
c      in over_button.   If we were, but now are not, we correct
c      the status of over_button.
c
      use allcomms
c
      if(zone_butt(mousex,mousey).gt.0
     *  )then
c
             kbutton=zone_butt(mousex,mousey)
c            write(*,109)kbutton
             over_button=.true.
         else
c            write(*,113)
c            if(over_button
c    *         )then
c                   write(*,112)kbutton
c            endif
             kbutton=-1
             over_button=.false.
      endif
c
      return
c
109   format('Button',i3,' found')
112   format('Button',i3,' left behind')
113   format('No button identified at this position')
c
      end
