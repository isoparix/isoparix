      subroutine draw_butt(nbutton)
c
c      Draws a button in a particular state, set by updown(nbutton)
c
      use allcomms
c
      character(2) direction
c
      logical bad_dir
c
      if(.not.exist_button(nbutton)
     *  )then
c
c      No point doing this!!
c
              write(0,103)nbutton
              return
      endif
c
      bad_dir=.true.
c
      if(nbutton.gt.ntop_button.or.
     *   nbutton.le.0
     *  )then
             write(0,100)nbutton,ntop_button
             call isoflush(0)
             return
      endif
c
      button_line=iybutton(nbutton)
      ixb=ixbutton(nbutton)
      ixtxt1=ixb+4
      iytxt= button_line+(butt_height/2)+5
      k=(updown(nbutton)+3)/2 ! Should give 2 or 1 from +1 and -1
c
      if(updown(nbutton).eq.1.or.
     *   updown(nbutton).eq.-1
     *  )then
             bad_dir=.false.
             ncol=butt_col(k,nbutton)
      endif
c
      if(bad_dir
     *  )then
             write(*,101)nbutton,updown(nbutton),k
             write(0,101)nbutton,updown(nbutton),k
             return
      endif
c
      lentext=len_trim(button_text(k,nbutton))
      ixtxt2=ixabutton(nbutton)
      ncol=float(ncol)*acols
c  
      write(8,102)nbutton,ixtxt1,ixtxt2,button_line,iytxt,lentext
     *           ,button_text(k,nbutton)
     *           ,ncol,acols,updown(nbutton)
      call isoflush(8)
      call x11drawbutton(%val(updown(nbutton)),%val(ixb)
     *                  ,%val(button_line)
     *         ,%val(ixtxt2),%val(button_line+butt_height),%val(ncol))
c  
      call x11text(%val(ixtxt1),%val(iytxt),button_text(k,nbutton)
     *            ,%val(0),%val(lentext))
c
      call x11updatezone(%val(ixb)   ,%val(button_line)
     *                  ,%val(ixtxt2),%val(button_line+butt_height))
c     call microsleep(100000)
c
      return
c
100   format('***** ERROR: DRAW_BUTT - invalid value for button',i3
     *      ,': ntop_button has been set to',i4)
101   format('***** ERROR: DRAW_BUTT - invalid direction for button',i3
     *      ,':  updown(nbutton)=',i3,', k=',i3)
102   format('DRAW_BUTT: nbutton=',i6,', ixtxt1=',i6,', ixtxt2=',i6
     *      ,', button_line=',i6,', iytxt=',i6,', lentext=',i6
     *      ,/', button_text(k,nbutton)=',a,', ncol=',i4,', acols='
     *      ,f8.1,', updown=',i4)
103   format('***** ERROR: DRAW_BUTT - button',i3,' does not exist')
c
      end
