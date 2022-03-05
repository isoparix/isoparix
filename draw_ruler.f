      subroutine draw_ruler(iyrow1,iyrow2)
c
      use allcomms
c
c     write(*,101)iyrow1,iyrow2,ndy1,ndy2,ndx1,ndx2
      if(lmxold.ne.last_mousex.or.
     *   ndy1.ne.iyrow1
     *  )then
             if(iyrow1.gt.0.and.iyrow2.gt.0
     *         )then
c
c      Reset old ruler zone...
c
c                   write(*,100)iyrow1,iyrow2,ndy1,ndy2,ndx1,ndx2
c    *                         ,disp_top,disp_bot
                    call x11updatezone(
     *                                  %val(ndx1)
     *                                 ,%val(disp_top)
     *                                 ,%val(ndx2)
     *                                 ,%val(disp_bot)
     *                                 )
c
c      Capture/change enduring ruler characteristics...
c
                    ndx1=last_mousex-ndelta_x
                    ndx2=last_mousex+ndelta_x
                    ndy1=iyrow1
                    ndy2=iyrow2
                    lmxold=last_mousex
             endif
c
c      Draw new ruler
c
             call x11ruler(
     *                     %val(last_mousex)
     *                    ,%val(disp_top)
     *                    ,%val(disp_bot)
     *                    ,%val(ndy1)
     *                    ,%val(ndy2)
     *                    ,%val(ndx1)
     *                    ,%val(ndx2)
     *                    )
             ruler=.true.
      endif
c
      return
c
100   format(/
     *       '****** RESET IN DRAW_RULER ***** '
     *     ,/'New row lines are:',2i6
     *     ,/'Old row lines are:',2i6
     *     ,/'Old  X-coords are:',2i6
     *     ,/'Display top/bottom',2i6
     *      )
101   format(/
     *       '****** ENTRY TO DRAW_RULER ***** '
     *     ,/'iyrow1/2:',2i6
     *     , ', ndy1/2:',2i6
     *     , ', ndx1/2:',2i6
     *      )
      end
