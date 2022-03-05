      subroutine button_setup(prog_name)
c
      use allcomms
      use dcv_comms
c
      character (1) hash
      character (7) prog_name
      character (20) butt_name,file_name
      character (100) text_input
c
      logical unmade(max_buttons)
c
      exist_button=.false.
      unmade=.false.
c
      do nbutton=1,max_buttons
         write(file_name,100)trim(adjustl(prog_name)),nbutton
c        write(*,*)file_name
         open(19,file=file_name,status='old',err=98)
         ntop_button=nbutton
         read(19,101)hash,text_input
c        write(*,102)n,text_input
         if(trim(text_input).ne.'#'
     *     )then
c
c      This button is defined.   Read its vertical position and
c      create it.
c
                read(text_input,104,iostat=ios,err=97)nrow
     *                               ,(butt_col(mx,nbutton),mx=2,1,-1)
     *                            ,(button_text(mx,nbutton),mx=2,1,-1)
                    iybutton(nbutton)=nrow*ivsepbutt
                exist_button(nbutton)=.true.
                      unmade(nbutton)=.true.
                      updown(nbutton)=1
                go to 99
         endif
 97      continue
c        write(0,105)text_input
         go to 99
 98      continue
c        write(8,106)file_name
 99      continue
         close(19)
      enddo
c
      do nbutton=1,ntop_button
         if(unmade(nbutton)
     *     )then
                next_butt_x=leftbutt-ihsepbutt
                ml=iybutton(nbutton)
c
                do n=1,ntop_button
                   if(iybutton(n).eq.ml
     *               )then
                          next_butt_x=next_butt_x+ihsepbutt
                          call make_button(n)
                          unmade(n)=.false.
                   endif
                enddo
         endif
c
      enddo
c
      return
c
100   format('key_act_',a7,'.',i2.2)
101   format(a1,1x,a)
102   format('BUTTON_SETUP: Button',i3,' is ',a)
103   format(a,' F',i0)
104   format(3i4,2a)
105   format('BUTTON_SETUP: Button line not defined for ',a)
106   format('BUTTON_SETUP: warning - cannot read file ',a)
c
      end
