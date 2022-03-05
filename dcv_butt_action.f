      subroutine dcv_butt_action
c
      use allcomms
      use dcv_comms
c
c     write(*,*)updown(kbutton)
      ml=iybutton(kbutton)
      nb=0
c
      do i=1,ntop_button
         if(ml.eq.iybutton(i)
     *     )then
                nb=nb+1
         endif
      enddo
c
      if(nb.eq.1.or.
     *   updown(kbutton).gt.0
     *  )then
             updown(kbutton)=-updown(kbutton)
             call draw_butt(kbutton)
      endif
c
      if(kbutton.eq.2.and.
     *   updown(kbutton).eq.1
     *  )then
             do i=3,ntop_button
                updown(i)=1
                call draw_butt(i)
             enddo
             call x11flush()
             return
      endif
c
      if(updown(kbutton).eq.-1
     *  )then
c
c      Toggle up all the other buttons in this line..
c
             do i=1,ntop_button
                if(i.ne.kbutton.and.
     *             ml.eq.iybutton(i)
     *            )then
                       updown(i)=1
                       call draw_butt(i)
                endif
             enddo
      endif
c
      call x11flush()
      return
      end
