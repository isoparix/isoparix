      subroutine arrows(nbut)
c
c      See how we can respond to arrow keys....
c
      use allcomms
c
c      Left arrow....
c
             if(nbut.eq.-800
     *         )then
                    if(mousex.gt.nhsp
     *                )then
                           mousex=mousex-1
                           nbut=-994
                    endif
             endif
c
c      Right arrow....
c
             if(nbut.eq.-802
     *         )then
                    if(mousex.lt.nhsp+nendcol
     *                )then
                           mousex=mousex+1
                           nbut=-994
                    endif
             endif
c
c      We want up and down arrows to change rows, not just mousex/y values
c
c      Up arrow....
c
             if(nbut.eq.-801
     *         )then
                    if(nrow_ruler.gt.1
     *                )then
                           mousey=line_y(nrow_ruler-1)+(iya/2)
                           nbut=-994
                    endif
             endif
c
c      Down arrow....
c
             if(nbut.eq.-803
     *         )then
                    if(nrow_ruler.lt.nrows
     *                )then
                           mousey=line_y(nrow_ruler+1)
                           if(mousey.ge.disp_bot
     *                       )then
                                  mousey=disp_bot-1
                           endif
                           nbut=-994
                    endif
             endif
c
      return
c
      end
