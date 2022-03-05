      subroutine make_button(nbutton)
c
c      Sets up a button with top left corner of ixb,button_line
c
      use allcomms
c
      ixb=next_butt_x
      ixbutton(nbutton)=ixb
c
      i=len_trim(button_text(1,nbutton))
      j=len_trim(button_text(2,nbutton))
      k=max0(i,j)
      ixc=8+ixb+(8*k)
      ixabutton(nbutton)=ixc
c     write(0,*)nbutton,ixb,ixc,iybutton(nbutton)
c    *         ,iybutton(nbutton)+butt_height
c
c      Check bounds....
c
      if(ixc.gt.ixm
     *  )then
             write(0,100)ixc,ixm
             ixc=ixm
      endif
c
      if(iybutton(nbutton).gt.iym
     *  )then
             write(0,101)iybutton(nbutton),iym
             iybutton(nbutton)=iym
      endif
c
      do j=iybutton(nbutton),iybutton(nbutton)+butt_height
         do i=ixb,ixc
            zone_butt(i,j)=nbutton
         enddo
      enddo
c
      call draw_butt(nbutton)
c
      next_butt_x=ixc+5
c
      return
c
100   format('MAKE_BUTTON: IXC too big. Changed from',i12,' to',i6)
101   format('MAKE_BUTTON: IYC too big. Changed from',i12,' to',i6)
      end
